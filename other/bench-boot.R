# benchmark boot_ci()

library(tidyverse)
library(microbenchmark)
library(rwmisc)

size  <- 10^(1:3)
times <- 10^(1:5)

# funs --------------------------------------------------------------------

# (hopefully) same output as boot_ci(), except can't use weights?
boot_ci_mat <- function(x, times, type = c("continuous", "discrete"), probs = c(0.1, 0.5, 0.9), w = 1) {
  stopifnot(is.numeric(times), length(times) == 1)
  stopifnot(is.numeric(probs), all(probs >= 0 & probs <= 1))
  type <- match.arg(type)

  x_len <- length(x)
  w_len <- length(w)

  if (w_len != 1 & x_len != w_len) {
    stop("`x` and `w` must have the same length.", call. = FALSE)
  }
  if (w_len == 1) {
    w <- rep(1, x_len)
  } else {
    w <- w / mean(w)
  }

  # ~~~~~ start diffs
  size <- x_len * times
  xs <- matrix(sample(x, size = size, replace = TRUE), nrow = x_len, ncol = times)
  # ~~~~~ end diffs

  if (type == "continuous") {
    means <- colMeans(xs)
    out <- quantile(means, probs = probs, names = FALSE)
    out <- list(mean = setNames(out, paste0("p", probs * 100)))
  } else if (type == "discrete") {
    vals <- sort(unique(x))
    out <- lapply(vals, function(val) {
      means <- colMeans(xs == val)
      res <- quantile(means, probs = probs, names = FALSE)
      setNames(res, paste0("p", probs * 100))
    })
    out <- setNames(out, vals)
  }

  out
}

bench_funs <- function(f_vapply, f_mat, size, times) {
  x <- runif(size)

  res <- microbenchmark::microbenchmark(
    f_vapply(x, times),
    f_mat(x, times),
    times = 10
  )

  res$size <- size
  res$times <- times
  res$time <- res$time / 1e9
  res
}

run_benches <- function(f_vapply, f_mat, size, times) {
  params <- tidyr::crossing(size, times)

  res <- purrr::pmap(params, function(size, times) {
    bench_funs(
      f_vapply = f_vapply,
      f_mat = f_mat,
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

# run ---------------------------------------------------------------------

res <- run_benches(
  f_vapply = boot_ci,
  f_mat = boot_ci_mat,
  size = size,
  times = times
)

plot_medians(res, x = "size", facet = "times")
