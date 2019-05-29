#' Alternative to tabulate
#'
#' @param x Vector
#' @return List of unique values of x and counts of each unique value
#' @export
tabulate2 <- function(x) {
  keys <- sort(unique(x))
  counts <- tabulate(match(x, keys))
  list(key = keys, count = counts)
}
