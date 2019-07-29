# benchmark weighted quantile

library(tidyverse)
library(rwmisc)
library(Hmisc)
library(bench)

sizes <- 10^(4:6)
digits <- 1:6
iterations <- 5

# funs --------------------------------------------------------------------

run_bench <- function(sizes, digits, iterations) {
  params <- tidyr::crossing(
    size = sizes,
    digits = digits
  )

  out <- lapply(seq_len(nrow(params)), function(i) {
    size <- params$size[[i]]
    digits <- params$digits[[i]]
    x <- round(rnorm(size), digits)
    w <- runif(size)
    uniques <- length(unique(x))

    res <- bench::mark(
      Hmisc::wtd.quantile(x, w),
      rwmisc::wtd_quantile(x, w),
      median(x),
      weighted.mean(x, w),
      mean(x),
      check = FALSE,
      iterations = iterations
    )

    res <- res[, c("expression", "median", "n_itr")]
    res$expression <- as.character(res$expression)
    res$median <- as.numeric(res$median)
    res$size <- size
    res$uniques <- uniques
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

res <- run_bench(sizes = sizes, digits = digits, iterations = iterations)

plot_bench(res, x = "uniques", facet = "size")
