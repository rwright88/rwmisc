# Benchmark bootstrap confidence intervals

library(bench)
library(dplyr)
library(ggplot2)

library(rwmisc)

sizes <- 10^(1:3)
times <- 10^(3:4)
iters <- 25

# funs --------------------------------------------------------------------

run_bench <- function(sizes, times, iters) {
  params <- expand.grid(size = sizes, times = times)

  out <- lapply(seq_len(nrow(params)), function(i) {
    size <- params$size[[i]]
    times <- params$times[[i]]
    x <- runif(size)
    w <- rep(1, size)

    res <- bench::mark(check = FALSE, iterations = iters,
      "unweighted" = rwmisc::boot_ci(x, times),
      "weighted"   = rwmisc::boot_ci(x, times, w = w)
    )

    res <- res[, c("expression", "median", "n_itr")]
    res$expression <- as.character(res$expression)
    res$median <- as.numeric(res$median)
    res$size <- size
    res$times <- times
    res
  })

  dplyr::bind_rows(out)
}

plot_bench <- function(data, x, facet = NULL) {
  x <- ggplot2::sym(x)
  data$expression <- reorder(data$expression, -data$median)

  out <- ggplot2::ggplot(data, ggplot2::aes(!!x, median, color = expression)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_x_log10(breaks = 10^(-10:10), minor_breaks = NULL) +
    ggplot2::scale_y_log10(breaks = 10^(-10:10), minor_breaks = NULL) +
    ggplot2::scale_color_brewer(type = "qual", palette = "Set1") +
    ggplot2::annotation_logticks() +
    ggplot2::theme_bw()

  if (!is.null(facet)) {
    out <- out + ggplot2::facet_wrap(facet, nrow = 1)
  }

  out
}

# run ---------------------------------------------------------------------

res <- run_bench(sizes = sizes, times = times, iters = iters)

plot_bench(res, x = "size", facet = "times")
