# bench split

library(dplyr)
library(data.table)

n <- 1e6
a <- sample(10, n, replace = TRUE)
b <- sample(10, n, replace = TRUE)
x <- runif(n)
dat <- tibble(a, b, x)
dt <- as.data.table(dat)
by <- c("a", "b")

# funs --------------------------------------------------------------------

f_split <- function(data, by) {
  out <- split(data, data[by])
  out
}

# unfinished
f_floop <- function(data, by) {
  uniques <- lapply(by, function(.x) {
    unique(data[[.x]])
  })

  len <- prod(vapply(uniques, FUN.VALUE = integer(1), FUN = length))
  out <- vector("list", len)
  out
}

f_dtab1 <- function(data, by) {
  out <- split(data, by = by)
  out
}

# unfinished
f_dtab2 <- function(data, by) {
  out <- data[, .(dts = list(a, b, x)), by]
  out
}

f_dplyr <- function(data, by) {
  out <- group_split(dat, !!!syms(by))
  out
}

# run ---------------------------------------------------------------------

bench::mark(
  f_split(dat, by),
  f_dtab1(dt, by),
  f_dplyr(dat, by),
  check = FALSE,
  iterations = 5
)
