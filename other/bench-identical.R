# benchmark identical_all()

library(bench)
library(rwmisc)

x <- runif(1e6)
y <- x
l <- list(x, y)

bench::mark(
  identical(x, y),
  identical_all(l)
)
