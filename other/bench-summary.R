# benchmark summary2()

library(bench)
library(dplyr)
library(ggplot2)
library(rwmisc)
library(tidyr)
library(vroom)

n_rows <- 10^(3:6)
n_cols <- c(4, 40)
iterations <- 25

# funs --------------------------------------------------------------------

f_base <- function(data) {
  summary(data)
}

f_rwmisc <- function(data) {
  rwmisc::summary2(data)
}

run_bench <- function(n_rows, n_cols, iterations) {
  params <- crossing(
    n_rows = n_rows,
    n_cols = n_cols
  )

  out <- lapply(seq_len(nrow(params)), function(i) {
    n_rows <- params$n_rows[[i]]
    n_cols <- params$n_cols[[i]]
    types <- strrep("lidc", round(n_cols / 4))
    data <- vroom::gen_tbl(n_rows, n_cols, col_types = types, missing = 0.1)

    res <- bench::mark(
      f_base(data),
      f_rwmisc(data),
      check = FALSE,
      iterations = iterations
    )

    res <- res[, c("expression", "median", "n_itr")]
    res$expression <- as.character(res$expression)
    res$median <- as.numeric(res$median)
    res$n_rows <- n_rows
    res$n_cols <- n_cols
    res
  })

  bind_rows(out)
}

plot_bench <- function(data, x, facet) {
  x_ <- sym(x)

  data %>%
    mutate(expression = reorder(expression, desc(median))) %>%
    ggplot(aes(!!x_, median, color = expression)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    facet_wrap(facet, nrow = 1) +
    scale_x_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_y_log10(breaks = 10 ^ (-10:10), minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    annotation_logticks() +
    theme_bw()
}

# run ---------------------------------------------------------------------

res <- run_bench(n_rows = n_rows, n_cols = n_cols, iterations = iterations)

plot_bench(res, x = "n_rows", facet = "n_cols")
