# benchmark boot_ci()

library(tidyverse)
library(bench)
library(rwmisc)

size <- round(round(10^seq(1, 3, 0.2)))
times <- 1e4

# funs --------------------------------------------------------------------

bench_funs <- function(boot_ci, size, times) {
  x <- runif(size)

  res <- bench::mark(
    boot_ci(x, times),
    iterations = 30,
    check = FALSE
  )

  out <- res[, c("expression", "median", "n_itr")]
  out$median <- as.numeric(res$median)
  out$size <- size
  out$times <- times
  out
}

run_benches <- function(boot_ci, size, times) {
  params <- tidyr::crossing(size, times)

  res <- purrr::pmap(params, function(size, times) {
    bench_funs(
      boot_ci = boot_ci,
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
    ggplot(aes(!!x, median, color = expression)) +
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

res <- run_benches(
  boot_ci = boot_ci,
  size = size,
  times = times
)

plot_medians(res, x = "size", facet = "times")
