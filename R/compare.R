#' Test that all elements in a vector are equal
#'
#' @param x Vector
#' @param na.rm Logical, remove NAs before comparing?
#' @param tol Tolerance
#' @return Logical
#' @export
equal_all <- function(x, na.rm = FALSE, tol = sqrt(.Machine$double.eps)) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  switch(typeof(x),
    logical   = (abs(max(x) - min(x)) < tol),
    integer   = (abs(max(x) - min(x)) < tol),
    double    = (abs(max(x) - min(x)) < tol),
    character = all(x == x[1]),
    list      = equal_list(x),
    stop("Unsupported type.", call. = FALSE)
  )
}

equal_list <- function(x) {
  res <- vector("logical", length(x))
  for (i in seq_along(res)) {
    res[i] <- identical(x[[1]], x[[i]])
  }
  all(res)
}
