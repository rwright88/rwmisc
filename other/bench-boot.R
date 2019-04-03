# benchmark boot_ci()

library(dplyr)
library(bench)
library(rwmisc)

x <- runif(1e3)
times <- 1e4

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

  reps <- seq_len(times)
  temp <- vector(typeof(x), x_len)

  size <- x_len * times

  xs <- matrix(sample(x, size = size, replace = TRUE), nrow = x_len, ncol = times)

  if (type == "continuous") {
    means <- colMeans(xs)
    out <- quantile(means, probs = probs, names = FALSE)
    out <- setNames(out, paste0("p", probs * 100))
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

bench::mark(check = FALSE, iterations = 10,
  boot_ci(x, times),
  boot_ci_mat(x, times)
)
