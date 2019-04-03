# TODO
# boot_ci() is unfinished
# https://ocw.mit.edu/courses/mathematics/18-05-introduction-to-probability-
#   and-statistics-spring-2014/readings/MIT18_05S14_Reading24.pdf

#' Calculate empirical bootstrap confidence intervals
#'
#' @param x Numeric vector
#' @param times Number of samples
#' @param type continuous or discrete
#' @param probs Numeric vector of probabilities
#' @param w Weight vector
#' @return List of confidence intervals
#' @export
boot_ci <- function(x, times, type = c("continuous", "discrete"), probs = c(0.1, 0.5, 0.9), w = 1) {
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

  xs <- vapply(reps, FUN.VALUE = temp, FUN = function(rep) {
    sample(x, size = x_len, prob = w, replace = TRUE)
  })

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
