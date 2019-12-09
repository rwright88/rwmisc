# benchmark boot_ci()

library(bench)
library(dplyr)
library(ggplot2)
library(rwmisc)
library(tidyr)

sizes <- 10^(1:3)
times <- 10^(3:4)
iterations <- 25

# funs --------------------------------------------------------------------

run_bench <- function(sizes, times, iterations) {
  params <- tidyr::crossing(
    size = sizes,
    times = times
  )

  out <- lapply(seq_len(nrow(params)), function(i) {
    size <- params$size[[i]]
    times <- params$times[[i]]
    x <- runif(size)

    res <- bench::mark(
      "rwmisc" = rwmisc::boot_ci(x, times),
      iterations = iterations
    )

    res <- res[, c("expression", "median", "n_itr")]
    res$expression <- as.character(res$expression)
    res$median <- as.numeric(res$median)
    res$size <- size
    res$times <- times
    res
  })

  dplyr::bind_rows(out)
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

res <- run_bench(sizes = sizes, times = times, iterations = iterations)

plot_bench(res, x = "size", facet = "times")
