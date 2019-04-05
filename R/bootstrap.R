#' Calculate empirical bootstrap confidence intervals
#'
#' @param x Numeric vector
#' @param times Number of bootstrap samples
#' @param fun Function to use
#' @param probs Numeric vector of probabilities
#' @param w Weight vector
#' @return Numeric vector of confidence intervals
#' @export
boot_ci <- function(x, times, fun = "mean", probs = c(0.1, 0.5, 0.9), w = 1) {
  stopifnot(is.numeric(x), length(x) > 1, anyNA(x) == FALSE)
  stopifnot(is.numeric(times), length(times) == 1)
  stopifnot(is.numeric(probs), all(probs >= 0 & probs <= 1))
  stopifnot(is.numeric(w), anyNA(w) == FALSE)
  fun <- match.fun(fun)

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

  res <- vapply(reps, FUN.VALUE = double(1), function(rep) {
    xs <- sample(x, size = x_len, replace = TRUE, prob = w)
    fun(xs)
  })

  out <- mean(res) + qnorm(p = probs) * sqrt(var(res))
  out <- setNames(out, paste0("p", probs * 100))
  out
}

#' Calculate empirical bootstrap confidence intervals
#'
#' @param x Numeric vector
#' @param times Number of bootstrap samples
#' @param fun Function to use
#' @param probs Numeric vector of probabilities
#' @param w Weight vector
#' @return Numeric vector of confidence intervals
#' @export
boot_ci2 <- function(x, times, fun = "mean", probs = c(0.1, 0.5, 0.9), w = 1) {
  stopifnot(is.numeric(x), length(x) > 1, anyNA(x) == FALSE)
  stopifnot(is.numeric(times), length(times) == 1)
  stopifnot(is.numeric(probs), all(probs >= 0 & probs <= 1))
  stopifnot(is.numeric(w), anyNA(w) == FALSE)
  fun <- match.fun(fun)

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

  size <- x_len * times

  if (size < 1e8) {
    samps <- sample(x, size = size, replace = TRUE, prob = w)
    xs <- matrix(samps, nrow = x_len, ncol = times)
    res <- vector("double", times)
    for (i in seq_along(res)) {
      res[i] <- fun(xs[, i])
    }
  } else {
    reps <- seq_len(times)
    res <- vapply(reps, FUN.VALUE = double(1), function(rep) {
      xs <- sample(x, size = x_len, replace = TRUE, prob = w)
      fun(xs)
    })
  }

  out <- mean(res) + qnorm(p = probs) * sqrt(var(res))
  out <- setNames(out, paste0("p", probs * 100))
  out
}
