# compare summary of data frames

library(bench)
library(dplyr)
library(rwmisc)
library(skimr)

n <- 1e6

dat <- tibble(
  l = sample(c(TRUE, FALSE), n, replace = TRUE),
  i = sample(n),
  d = runif(n),
  c = sample(letters, n, replace = TRUE)
)

dat

summary(dat)
summary2(dat)
skimr::skim(dat)

bench::mark(
  summary(dat),
  summary2(dat),
  skim(dat),
  check = FALSE,
  iterations = 5
)
