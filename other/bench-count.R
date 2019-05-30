# benchmark count

library(tidyverse)
library(rwmisc)
library(vctrs)
library(bench)

sizes <- 10^(5:7)
upper <- 10^(1:7)

# funs --------------------------------------------------------------------

run_bench <- function(sizes, upper) {
  params <- tidyr::crossing(
    size = sizes,
    upper = upper
  )

  out <- lapply(seq_len(nrow(params)), function(i) {
    size <- params$size[[i]]
    upper <- params$upper[[i]]
    x <- as.integer(round(runif(size, min = -upper, max = upper)))
    df <- dplyr::tibble(x = x)

    res <- bench::mark(
      sum(x),
      tabulate(x),
      rwmisc::count(x),
      vctrs::vec_count(x),
      dplyr::count(df, x),
      table(x),
      check = FALSE,
      iterations = 1
    )

    res <- res[, c("expression", "median", "n_itr")]
    res$median <- as.numeric(res$median)
    res$size <- size
    res$upper <- upper
    res
  })

  out <- bind_rows(out)
  out
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
    theme_rw()
}

# run ---------------------------------------------------------------------

res <- run_bench(sizes = sizes, upper = upper)

plot_bench(res, x = "upper", facet = "size")
