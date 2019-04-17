# bench sample

library(tidyverse)
library(bench)

# funs --------------------------------------------------------------------

sample1 <- function(x, times) {
  x_len <- length(x)
  reps <- seq_len(times)
  xs <- vapply(reps, FUN.VALUE = numeric(x_len), function(rep) {
    sample(x, size = x_len, replace = TRUE)
  })
  xs
}

sample2 <- function(x, times) {
  x_len <- length(x)
  size <- x_len * times
  samps <- sample(x, size = size, replace = TRUE)
  xs <- matrix(samps, nrow = x_len, ncol = times)
  xs
}

run_bench <- function() {
  params <- tidyr::crossing(
    x_len = 10^(1:4),
    times = 10^(3:5)
  )
  params <- params[params$x_len * params$times <= 1e7, ]

  out <- lapply(seq_len(nrow(params)), function(i) {
    x_len <- params$x_len[[i]]
    times <- params$times[[i]]
    x <- runif(x_len)

    res <- bench::mark(
      sample1(x, times),
      sample2(x, times),
      check = FALSE
    )
    res <- res[, c("expression", "median", "n_itr")]
    res$median <- as.numeric(res$median)
    res$x_len <- x_len
    res$times <- times
    res
  })

  out <- dplyr::bind_rows(out)
  out
}

plot_bench <- function(data) {
  data %>%
    ggplot(aes(x_len, median, color = expression)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    facet_wrap("times", nrow = 1) +
    scale_x_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_y_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    annotation_logticks() +
    rwmisc::theme_rw()
}

# run ---------------------------------------------------------------------

res <- run_bench()

plot_bench(res)
