# benchmark count
# TODO
# https://github.com/traversc/trqwe

library(bench)
library(dplyr)
library(ggplot2)
library(tidyr)

library(rwmisc)
library(vctrs)

sizes <- 10^(5:6)
d_uniques <- 10^(-4:-1)
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

run_bench <- function(sizes, d_uniques, iterations) {
  params <- crossing(
    size = sizes,
    d_uniques = d_uniques
  )

  out <- lapply(seq_len(nrow(params)), function(i) {
    size <- params$size[[i]]
    d_uniques <- params$d_uniques[[i]]
    n_uniques <- round(size * d_uniques)
    x <- sample(seq_len(n_uniques), size, replace = TRUE)
    df <- tibble(x = x)

    res <- bench::mark(
      "table"  = table(x),
      "fmatch" = rwmisc::count(x),
      "match"  = count_match(x),
      "vctrs"  = vctrs::vec_count(x),
      "dplyr"  = dplyr::count(df, x),
      check = FALSE,
      iterations = iterations
    )

    res <- res[, c("expression", "median", "n_itr")]
    res$expression <- as.character(res$expression)
    res$median <- as.numeric(res$median)
    res$size <- size
    res$d_uniques <- d_uniques
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

res <- run_bench(sizes = sizes, d_uniques = d_uniques, iterations = iterations)

plot_bench(res, x = "d_uniques", facet = "size")
