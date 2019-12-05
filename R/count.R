#' Alternative to tabulate
#'
#' @param x Vector
#' @param sort Logical, sort in descending order of count?
#' @return List of unique values of x and counts of each unique value
#' @export
count <- function(x, sort = FALSE) {
  if (is.factor(x)) {
    keys <- levels(x)
    counts <- tabulate(x)
  } else {
    keys <- sort(unique(x), na.last = FALSE)
    counts <- tabulate(fastmatch::fmatch(x, keys))
    keys <- as.vector(keys)
  }

  out <- list(key = keys, count = counts)
  if (sort == TRUE) {
    ord <- order(-out$count)
    out <- lapply(out, function(.x) .x[ord])
  }
  out
}
