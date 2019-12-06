#' Rolling exponential moving average
#'
#' @param x Numeric vector
#' @param n Window size
#' @param alpha Smoothing parameter
#' @param ... Addional arguments passed on to `roll::roll_mean()`
#' @return Numeric vector of size x
#' @export
roll_ema <- function(x, n, alpha = 0.1, ...) {
  stopifnot(is.numeric(n), n >= 1)
  stopifnot(is.numeric(alpha), alpha >= 0, alpha <= 1)
  w <- alpha * (1 - alpha) ^ ((n - 1):0)
  roll::roll_mean(x, n, weights = w, ...)
}
