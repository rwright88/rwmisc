# benchmark boot_ci()

library(dplyr)
library(bench)
library(rwmisc)

x <- runif(1e4)
times <- 1e4

f1 <- function(x, times) {
  x_len <- length(x)
  reps <- seq_len(times)
  temp <- vector(typeof(x), x_len)

  vapply(reps, FUN.VALUE = temp, FUN = function(rep) {
    sample(x, size = x_len, replace = TRUE)
  })
}

f2 <- function(x, times) {
  x_len <- length(x)

  matrix()
}

bench::mark(check = FALSE,
  f1(x, times),
  f2(x, times)
)
