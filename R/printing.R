#' Print summary2
#'
#' @param x Data frame
#' @param digits Minimum number of significant digits to use
#' @param n_rows Maximum number of rows to print
#' @param ... Other arguments passed on to print
#' @export
print.summary2 <- function(x, digits = 4, n_rows = 10, ...) {
  stopifnot(is.numeric(digits), length(digits) == 1)
  stopifnot(is.numeric(n_rows), length(n_rows) == 1)
  n_cells <- round(ncol(x) * n_rows)
  print.data.frame(x, digits = digits, max = n_cells)
  invisible(x)
}
