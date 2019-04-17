# benchmark matrix column operations

library(matrixStats)

mat <- matrix(runif(1e6), nrow = 1e2, ncol = 1e4)
iterations <- 5

# funs --------------------------------------------------------------------

f_colmeans <- function(x) {
  colMeans(x)
}

f_colmeans2 <- function(x) {
  matrixStats::colMeans2(x)
}

f_colmedians <- function(x) {
  matrixStats::colMedians(x)
}

f_floop <- function(x, fun) {
  fun <- match.fun(fun)
  out <- vector("double", ncol(x))
  for (i in seq_along(out)) {
    out[i] <- fun(x[, i])
  }
  out
}

f_vapply <- function(x, fun) {
  fun <- match.fun(fun)
  vapply(seq_len(ncol(x)), FUN.VALUE = double(1), FUN = function(i) {
    fun(x[, i])
  })
}

f_apply <- function(x, fun) {
  fun <- match.fun(fun)
  apply(x, MARGIN = 2, FUN = fun)
}

# run ---------------------------------------------------------------------

res_means <- bench::mark(
  f_colmeans(mat),
  f_colmeans2(mat),
  f_floop(mat, "mean"),
  f_vapply(mat, "mean"),
  f_apply(mat, "mean"),
  check = TRUE,
  iterations = iterations
)

res_means[, 1:9]

res_medians <- bench::mark(
  f_colmedians(mat),
  f_floop(mat, "median"),
  f_vapply(mat, "median"),
  f_apply(mat, "median"),
  check = TRUE,
  iterations = iterations
)

res_medians[, 1:9]
