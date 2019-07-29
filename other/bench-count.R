# benchmark count

library(bench)
library(dplyr)
library(ggplot2)
library(rwmisc)
library(tidyr)
library(vctrs)

sizes <- 10^(4:6)
upper <- 10^(1:6)
iterations <- 25

# funs --------------------------------------------------------------------

count_match <- function(x) {
  if (is.factor(x)) {
    keys <- levels(x)
    counts <- tabulate(x)
  } else {
    keys <- sort(unique(x), na.last = FALSE)
    counts <- tabulate(match(x, keys))
    keys <- as.vector(keys)
  }
  list(key = keys, count = counts)
}

run_bench <- function(sizes, upper, iterations) {
  params <- crossing(
    size = sizes,
    upper = upper
  )

  out <- lapply(seq_len(nrow(params)), function(i) {
    size <- params$size[[i]]
    upper <- params$upper[[i]]
    x <- as.integer(round(runif(size, min = -upper, max = upper)))
    df <- tibble(x = x)

    res <- bench::mark(
      rwmisc::count(x),
      count_match(x),
      vctrs::vec_count(x),
      dplyr::count(df, x),
      table(x),
      check = FALSE,
      iterations = iterations
    )

    res <- res[, c("expression", "median", "n_itr")]
    res$expression <- as.character(res$expression)
    res$median <- as.numeric(res$median)
    res$size <- size
    res$upper <- upper
    res
  })

  bind_rows(out)
}

plot_bench <- function(data, x, facet) {
  x_ <- sym(x)

  data %>%
    mutate(expression = reorder(expression, desc(median))) %>%
    ggplot(aes(!!x_, median, color = expression)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    facet_wrap(facet, nrow = 1) +
    scale_x_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_y_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    annotation_logticks() +
    theme_bw()
}

# run ---------------------------------------------------------------------

res <- run_bench(sizes = sizes, upper = upper, iterations = iterations)

plot_bench(res, x = "upper", facet = "size")
