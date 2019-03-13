# other types and classes for summary2()

x <- list(
  "TRUE" = TRUE,
  "1L" = 1L,
  "1.1" = 1.1,
  "a" = "a",
  "complex(1, 1, 1)" = complex(1, 1, 1),
  "raw(1)" = raw(1),

  "factor(1)" = factor(1),
  "Sys.Date()" = Sys.Date(),
  "lubridate::today()" = lubridate::today(),
  "Sys.time()" = Sys.time(),
  "lubridate::now()" = lubridate::now(),
  # zoo,
  # xts,

  "list(1)" = list(1)
)

lapply(x, typeof)
lapply(x, class)

df <- dplyr::as_tibble(x)
dplyr::glimpse(df)
rwmisc::summary2(df)
