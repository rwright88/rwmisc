#' Alternative to tabulate
#'
#' @param x Numeric vector
#' @return Numeric vector of counts of unique values of x
#' @export
tabulate2 <- function(x) {
  stopifnot(is.numeric(x))
  if (typeof(x) != "integer") {
    x <- as.integer(x)
  }

  nas <- sum(is.na(x))
  zeros <- sum(x == 0, na.rm = TRUE)

  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)

  if (x_min < 0) {
    neg <- rev(tabulate(abs(x[x < 0])))
  } else {
    neg <- NULL
  }

  if (zeros == 0) {
    zeros <- NULL
  }

  if (x_max > 0) {
    pos <- tabulate(x)
  } else {
    pos <- NULL
  }

  nms <- seq.int(min(x_min, 1), max(x_max, -1))
  nms <- c(NA, nms)

  out <- c(nas, neg, zeros, pos)
  out <- stats::setNames(out, nms)
  out <- out[out != 0]
  out
}
