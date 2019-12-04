#' Weighted quantiles
#'
#' @param x Numeric vector
#' @param w Numeric vector of weights
#' @param probs Numeric vector of probabilities
#' @param na.rm Logical
#' @return Numeric vector of length probs
#' @export
wtd_quantile <- function(x, w = NULL, probs = seq(0, 1, 0.25), na.rm = FALSE) {
  if (is.null(w)) {
    return(stats::quantile(x, probs = probs, na.rm = na.rm))
  }
  stopifnot(is.numeric(x), is.numeric(w), is.numeric(probs))
  stopifnot(length(x) == length(w))
  stopifnot(all(probs >= 0 & probs <= 1))

  if (na.rm == TRUE) {
    good <- !is.na(x + w)
    x <- x[good]
    w <- w[good]
  } else if (anyNA(x + w)) {
    stop("Missing values not allowed if `na.rm` is FALSE.", call. = FALSE)
  }

  zeros <- (w == 0)
  if (any(zeros)) {
    x <- x[!zeros]
    w <- w[!zeros]
  }

  if (length(x) == 0) {
    return(rep(NA_real_, length(probs)))
  }

  o <- order(x)
  x <- x[o]
  w <- w[o]
  n <- sum(w)
  cuts <- 1 + (n - 1) * probs
  stats::approx(cumsum(w), x, xout = cuts, method = "constant", rule = 2, f = 1)$y
}
