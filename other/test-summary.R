# other types and classes for summary2()

library(dplyr)
library(rwmisc)

n <- 1e6

x <- list(
  "lgl" = sample(c(TRUE, FALSE, NA), n, replace = TRUE),
  "int" = sample(c(1:10, NA), n, replace = TRUE),
  "dbl" = rnorm(n, 5, 1),
  "chr" = sample(c(letters[1:10], NA), n, replace = TRUE),
  "cpl" = complex(1, 1, 1),
  "raw" = raw(1),

  "fct" = factor(sample(c(letters[1:10], NA), n, replace = TRUE)),
  "Sys.Date()" = Sys.Date(),
  "lubridate::today()" = lubridate::today(),
  "Sys.time()" = Sys.time(),
  "lubridate::now()" = lubridate::now(),
  # zoo,
  # xts,

  "list" = list(1)
)

str(lapply(x, typeof))
str(lapply(x, class))

df <- dplyr::as_tibble(x)
dplyr::glimpse(df)
rwmisc::summary2(df)