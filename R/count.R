#' Alternative to tabulate
#'
#' @param x Vector
#' @return List of unique values of x and counts of each unique value
#' @export
count <- function(x) {
  keys <- sort(unique(x), na.last = FALSE)
  counts <- tabulate(fastmatch::fmatch(x, keys))
  keys <- as.vector(keys)
  list(key = keys, count = counts)
}
