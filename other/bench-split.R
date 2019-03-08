# bench split

library(dplyr)
library(data.table)

n <- 1e6
a <- sample(letters[1:10], n, replace = TRUE)
b <- sample(letters[1:10], n, replace = TRUE)
x <- runif(n)
dat <- tibble(a, b, x)

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

  # for

  out
}

f_dtab1 <- function(data, by) {
  data <- as.data.table(data)
  out <- split(data, by = by)
  out
}

f_dplyr <- function(data, by) {
  out <- group_split(dat, !!!syms(by))
  out
}

# run ---------------------------------------------------------------------

bench::mark(
  f_split(dat, c("a", "b")),
  # f_floop(dat, c("a", "b")),
  f_dtab1(dat, c("a", "b")),
  f_dplyr(dat, c("a", "b")),
  check = FALSE,
  iterations = 5
)
