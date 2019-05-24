# benchmark tabulate2

library(tidyverse)
library(rwmisc)
library(vctrs)
library(bench)

sizes <- 10^(4:6)
rates <- 10^(-5:0)

# funs --------------------------------------------------------------------

run_bench <- function(sizes, rates) {
  params <- tidyr::crossing(
    size = sizes,
    rate = rates
  )
  params <- filter(params, size * rate > 5)

  out <- lapply(seq_len(nrow(params)), function(i) {
    size <- params$size[[i]]
    rate <- params$rate[[i]]

    n_unique <- round(size * rate)
    vals <- seq(-n_unique / 2, n_unique / 2)
    x <- sample(vals, size = size, replace = TRUE)

    res <- bench::mark(
      sum(x),
      tabulate(x),
      tabulate2(x),
      vec_count(x),
      table(x),
      check = FALSE,
      iterations = 30
    )

    res <- res[, c("expression", "median", "n_itr")]
    res$median <- as.numeric(res$median)
    res$size <- size
    res$rate <- rate
    res
  })

  out <- bind_rows(out)
  out
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
    theme_rw()
}

# run ---------------------------------------------------------------------

res <- run_bench(sizes = sizes, rates = rates)

plot_bench(res, x = "rate", facet = "size")
