#' Test objects in list for exact equality
#'
#' @param x List of objects
#' @return A single logical value.
#' @export
identical_all <- function(x) {
  if (!is.list(x) || length(x) < 2) {
    stop("`x` must be a list of at least length 2.", call. = FALSE)
  }

  all(vapply(x[-1], FUN.VALUE = logical(1), FUN = function(.x) {
    identical(.x, x[[1]])
  }))
}
