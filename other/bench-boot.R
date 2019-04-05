# benchmark boot_ci()

library(tidyverse)
library(microbenchmark)
library(rwmisc)

size  <- 10^(1:3)
times <- 10^(4:5)

# funs --------------------------------------------------------------------

bench_funs <- function(boot_ci, boot_ci2, size, times) {
  x <- runif(size)

  res <- microbenchmark::microbenchmark(
    boot_ci(x, times),
    boot_ci2(x, times),
    times = 30
  )

  res$size <- size
  res$times <- times
  res$time <- res$time / 1e9
  res
}

run_benches <- function(boot_ci, boot_ci2, size, times) {
  params <- tidyr::crossing(size, times)

  res <- purrr::pmap(params, function(size, times) {
    bench_funs(
      boot_ci = boot_ci,
      boot_ci2 = boot_ci2,
      size = size,
      times = times
    )
  })

  res <- dplyr::bind_rows(res)
  res
}

plot_medians <- function(data, x, facet) {
  x <- sym(x)
  facet <- sym(facet)

  data %>%
    group_by(!!x, !!facet, expr) %>%
    summarise(time_p50 = median(time)) %>%
    ungroup() %>%
    ggplot(aes(!!x, time_p50, color = expr)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    facet_wrap(facet, nrow = 1) +
    scale_x_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_y_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    annotation_logticks() +
    rwmisc::theme_rw()
}

plot_times <- function(data, x, facet) {
  x <- sym(x)
  facet <- sym(facet)

  data %>%
    mutate(size = factor(size)) %>%
    ggplot(aes(!!x, time, color = expr)) +
    geom_boxplot(outlier.alpha = 0.5) +
    facet_wrap(facet, nrow = 1) +
    scale_y_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    annotation_logticks() +
    rwmisc::theme_rw()
}

# run ---------------------------------------------------------------------

res <- run_benches(
  boot_ci = boot_ci,
  boot_ci2 = boot_ci2,
  size = size,
  times = times
)

plot_medians(res, x = "size", facet = "times")
plot_times(res, x = "size", facet = "times")
