# compare summary of data frames

library(tidyverse)
library(rwmisc)
library(skimr)

# funs --------------------------------------------------------------------

create_data <- function(n_rows, reps) {
  dat <- map_dfc(seq_len(reps), function(.x) {
    tibble(
      logical = sample(c(TRUE, FALSE, NA), n_rows, replace = TRUE),
      integer = sample(c(1:10, NA), n_rows, replace = TRUE),
      double = rnorm(n_rows, mean = 5, sd = 1),
      character = sample(c(letters[1:10], NA), n_rows, replace = TRUE)
    )
  })

  dat
}

# run ---------------------------------------------------------------------

dat <- create_data(n_rows = 1e6, reps = 5)
dat

summary(dat)
summary2(dat)
skimr::skim(dat)

bench::mark(
  summary(dat),
  summary2(dat),
  skim(dat),
  check = FALSE,
  iterations = 3
)
