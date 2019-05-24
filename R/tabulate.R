#' Alternative to tabulate
#'
#' @param x Numeric vector
#' @return Data frame of unique values of x and counts of each unique value
#' @export
tabulate2 <- function(x) {
  stopifnot(is.numeric(x))
  if (typeof(x) != "integer") {
    x <- as.integer(x)
  }

  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)

  if (x_min < 0) {
    neg <- rev(tabulate(abs(x[x < 0])))
  } else {
    neg <- NULL
  }

  if (x_min > 0 || x_max < 0) {
    zeros <- NULL
  } else {
    zeros <- sum(x == 0, na.rm = TRUE)
  }

  if (x_max > 0) {
    pos <- tabulate(x)
  } else {
    pos <- NULL
  }

  keys <- seq.int(min(x_min, 1), max(x_max, -1))

  nas <- sum(is.na(x))
  if (nas > 0) {
    keys <- c(NA, keys)
  }

  counts <- c(nas, neg, zeros, pos)
  ind <- which(counts != 0)
  counts <- counts[ind]
  keys <- keys[ind]

  dplyr::tibble(key = keys, count = counts)
}
