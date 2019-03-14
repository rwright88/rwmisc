# benchmark summary2()

library(tidyverse)
library(skimr)
library(rwmisc)
library(microbenchmark)

n_rows <- 10^(3:6)

# funs --------------------------------------------------------------------

f_base <- function(data) {
  summary(data)
}

f_skimr <- function(data) {
  skimr::skim(data)
}

f_rwmisc <- function(data) {
  rwmisc::summary2(data)
}

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

bench_funs <- function(f_base, f_skimr, f_rwmisc, n_rows) {
  n_cols <- 100
  reps <- round(n_cols / 4)
  data <- create_data(n_rows = n_rows, reps = reps)

  res <- microbenchmark(
    f_base(data),
    f_skimr(data),
    f_rwmisc(data),
    times = 5
  )

  res <- mutate(res, n_rows, time = time / 1e9)
  res
}

run_benches <- function(f_base, f_skimr, f_rwmisc, n_rows) {
  res <- lapply(n_rows, function(.x) {
    bench_funs(
      f_base = f_base,
      f_skimr = f_skimr,
      f_rwmisc = f_rwmisc,
      n_rows = .x
    )
  })

  res <- dplyr::bind_rows(res)
  res
}

plot_medians <- function(data) {
  data %>%
    group_by(n_rows, expr) %>%
    summarise(time_p50 = median(time)) %>%
    ungroup() %>%
    ggplot(aes(n_rows, time_p50, color = expr)) +
    geom_point(size = 2) +
    geom_line(size = 1.1) +
    scale_x_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_y_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    annotation_logticks() +
    rwmisc::theme_rw()
}

# run ---------------------------------------------------------------------

dat <- create_data(n_rows = 1e5, reps = 2)
summary2(dat)

res <- run_benches(f_base, f_skimr, f_rwmisc, n_rows = n_rows)

plot_medians(res)
