# TODO
# bench split

library(tidyverse)

n <- 1e6
a <- sample(letters[1:10], n, replace = TRUE)
b <- sample(letters[1:10], n, replace = TRUE)
x <- runif(n)
dat <- tibble(a, b, x)

f1 <- function(data, by) {
  uniques <- vector("list", length(by))

  for (i in seq_along(uniques)) {
    by1 <- by[i]
    uniques[[i]] <- unique(data[[by1]])
  }

  uniques
}

f2 <- function() {
  split(dat, dat[c("a", "b")])
}

bench::mark(
  f1(),
  f2(),
  check = FALSE
)
