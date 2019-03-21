# benchmark summary2_by()

library(dplyr)

n <- 1e6

dat <- dplyr::tibble(
  c1 = sample(letters[1:10], n, replace = TRUE),
  c2 = sample(letters[1:10], n, replace = TRUE),
  c3 = sample(letters[1:10], n, replace = TRUE),
  c4 = sample(letters[1:10], n, replace = TRUE),
  d1 = runif(n)
)

bench::mark(
  summary(dat),
  summary2(dat),
  summary2_by(dat, c("c1", "c2", "c3", "c4"), "d1"),
  check = FALSE
)
