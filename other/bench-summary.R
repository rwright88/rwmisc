# compare summary of data frames

library(tidyverse)
library(rwmisc)
library(skimr)

# funs --------------------------------------------------------------------

create_data <- function(n_rows, reps) {
  dat <- purrr::map_dfc(seq_len(reps), function(.x) {
    tibble(
      logical = sample(c(TRUE, FALSE), n_rows, replace = TRUE),
      integer = sample(n_rows),
      double = runif(n_rows),
      character = sample(letters, n_rows, replace = TRUE)
    )
  })

  dat
}

# run ---------------------------------------------------------------------

dat <- create_data(n_rows = 1e6, reps = 3)
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
