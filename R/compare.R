# TODO
# NA?
# identical vs all.equal for lists
# additional arguments passed with ...
# add tests

#' Test that all elements in vector are equal
#'
#' @param x Vector
#' @param tol Tolerance
#' @return Logical
#' @export
equal <- function(x, tol = sqrt(.Machine$double.eps)) {
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
