# benchmark summary2()

library(tidyverse)
library(skimr)
library(rwmisc)
library(microbenchmark)

n_rows <- 10^(3:6)
n_cols <- 10^(1:2)

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
      lgl = sample(c(TRUE, FALSE, NA), n_rows, replace = TRUE),
      int = sample(c(1:10, NA), n_rows, replace = TRUE),
      dbl = rnorm(n_rows, mean = 5, sd = 1),
      chr = sample(c(letters[1:10], NA), n_rows, replace = TRUE)
    )
  })

  dat
}

bench_funs <- function(f_base, f_skimr, f_rwmisc, n_rows, n_cols) {
  reps <- round(n_cols / 4)
  data <- create_data(n_rows = n_rows, reps = reps)

  res <- microbenchmark(
    f_base(data),
    # f_skimr(data),
    f_rwmisc(data),
    times = 10
  )

  res <- mutate(res, n_rows, n_cols, time = time / 1e9)
  res
}

run_benches <- function(f_base, f_skimr, f_rwmisc, n_rows, n_cols) {
  params <- crossing(n_rows, n_cols)

  res <- pmap(params, function(n_rows, n_cols) {
    bench_funs(
      f_base = f_base,
      f_skimr = f_skimr,
      f_rwmisc = f_rwmisc,
      n_rows = n_rows,
      n_cols = n_cols
    )
  })

  res <- dplyr::bind_rows(res)
  res
}

plot_medians <- function(data, x, facet) {
  x <- sym(x)
  facet <- sym(facet)

  data %>%
    group_by(!!x, !!facet, expr) %>%
    summarise(time_p50 = median(time)) %>%
    ungroup() %>%
    ggplot(aes(!!x, time_p50, color = expr)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    facet_wrap(facet) +
    scale_x_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_y_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    annotation_logticks() +
    rwmisc::theme_rw()
}

# run ---------------------------------------------------------------------

dat <- create_data(n_rows = 1e6, reps = 1)
summary2(dat)

res <- run_benches(
  f_base = f_base,
  f_skimr = f_skimr,
  f_rwmisc = f_rwmisc,
  n_rows = n_rows,
  n_cols = n_cols
)

plot_medians(res, x = "n_rows", facet = "n_cols")
