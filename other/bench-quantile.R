# bench weighted quantile

library(rwmisc)
library(Hmisc)

n <- 1e6
x <- runif(n)
w <- round(rnorm(n, mean = 100, sd = 10))

wtd.quantile(x, w)
wtd_quantile(x, w)

identical(
  round(as.numeric(wtd.quantile(x, w)), 4),
  round(wtd_quantile(x, w), 4)
)

system.time(wtd.quantile(x, w))
system.time(wtd_quantile(x, w))

microbenchmark::microbenchmark(
  wtd.quantile(x, w),
  wtd_quantile(x, w),
  median(x),
  weighted.mean(x, w),
  mean(x),
  sum(x),
  times = 2
)
