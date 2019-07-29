# benchmark equal_all()

library(bench)
library(rwmisc)

n <- 1e6

# funs --------------------------------------------------------------------

bench_mark <- function(...) {
  bench::mark(..., iterations = 25)
}

equal_lgl <- function(x) {
  (all(x == TRUE) | all(x == FALSE))
}

# run ---------------------------------------------------------------------

x <- rep(TRUE, n)
bench_mark(
  equal_all(x),
  equal_lgl(x)
)

x <- rep(1L, n)
bench_mark(
  equal_all(x)
)

x <- rep(1.1, n)
bench_mark(
  equal_all(x)
)

x <- rep("a", n)
bench_mark(
  equal_all(x)
)

a <- as.list(rep(1, n / 2))
b <- as.list(rep("a", n / 2))
x <- c(a, b)
bench_mark(
  equal_all(x)
)
