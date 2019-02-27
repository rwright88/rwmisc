#' Test objects in list for exact equality
#'
#' @param x List of objects
#' @return A single logical value.
#' @export
identical_all <- function(x) {
  if (length(x) < 2) {
    stop("Length of x must be at least 2.", call. = FALSE)
  }

  all(vapply(x[-1], FUN.VALUE = logical(1), FUN = function(.x) {
    identical(.x, x[[1]])
  }))
}
