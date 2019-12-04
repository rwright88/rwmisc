# test summary2()

library(data.table)
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
  "date" = Sys.Date(),
  "datetime" = Sys.time(),
  "list" = list(1)
)

str(lapply(x, typeof))
str(lapply(x, class))

df <- data.table::as.data.table(x)
df

print(rwmisc::summary2(df), digits = 4)
