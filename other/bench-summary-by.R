# benchmark summary2_by()

library(dplyr)
library(bench)
library(profvis)
library(rwmisc)

n <- 1e6

dat <- dplyr::tibble(
  c1 = sample(letters[1:10], n, replace = TRUE),
  c2 = sample(letters[1:10], n, replace = TRUE),
  c3 = sample(letters[1:10], n, replace = TRUE),
  c4 = sample(letters[1:10], n, replace = TRUE),
  d1 = runif(n),
  d2 = runif(n)
)

# test 1 ------------------------------------------------------------------

f_dplyr <- function(data) {
  out <- group_by(dat, c1, c2, c3)
  out <- summarise(out,
    n = n(),
    d_na = mean(is.na(d1)),
    mean = mean(d1),
    q = list(quantile(d1))
  )

  out
}

bench::mark(iterations = 5, check = FALSE,
  summary(dat),
  summary2(dat),
  summary2_by(dat, by = c("c1", "c2", "c3"), vars = "d1"),
  f_dplyr(dat)
)

# test2 -------------------------------------------------------------------

profvis(
  summary2_by(dat, by = c("c1", "c2", "c3", "c4"), vars = c("d1", "d2"))
)
