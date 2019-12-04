#' Calculate empirical bootstrap confidence intervals
#'
#' @param x Numeric vector
#' @param times Number of bootstrap samples
#' @param fun Character vector of function to use, default is mean
#' @param probs Numeric vector of probabilities
#' @param w Numeric weight vector
#' @return Numeric vector of confidence intervals
#' @export
boot_ci <- function(x, times, fun = "mean", probs = c(0.1, 0.5, 0.9), w = NULL) {
  stopifnot(is.numeric(x), anyNA(x) == FALSE)
  stopifnot(is.numeric(times), length(times) == 1)
  stopifnot(is.numeric(probs), all(probs >= 0 & probs <= 1))

  x_len <- length(x)
  if (x_len < 1) {
    out <- rep(NA_real_, length(probs))
    out <- set_names(out, paste0("p", probs * 100))
    return(out)
  }

  size <- x_len * times
  size_max <- 1e7
  fun_mat <- get_matrix_fun(fun, size, size_max)
  fun <- match.fun(fun_mat$fun)

  if (!is.null(w)) {
    stopifnot(is.numeric(w), anyNA(w) == FALSE)
    w_len <- length(w)
    if (x_len != w_len) {
      stop("`x` and `w` must have the same length.", call. = FALSE)
    }
    w <- w / mean(w)
  }

  if (size <= size_max) {
    samps <- sample(x, size = size, replace = TRUE, prob = w)
    xs <- matrix(samps, nrow = x_len, ncol = times)
    if (fun_mat$is_matrix_fun == TRUE) {
      res <- fun(xs)
    } else {
      res <- vector("double", times)
      for (i in seq_along(res)) {
        res[i] <- fun(xs[, i])
      }
    }
  } else {
    reps <- seq_len(times)
    res <- vapply(reps, FUN.VALUE = double(1), function(rep) {
      xs <- sample(x, size = x_len, replace = TRUE, prob = w)
      fun(xs)
    })
  }

  out <- stats::qnorm(p = probs, mean = mean(res), sd = sqrt(stats::var(res)))
  out <- set_names(out, paste0("p", probs * 100))
  out
}

get_matrix_fun <- function(fun, size, size_max) {
  stopifnot(is.character(fun), length(fun) == 1)

  if (size > size_max) {
    return(list(fun = fun, is_matrix_fun = FALSE))
  }

  switch(fun,
    mean = list(fun = "colMeans", is_matrix_fun = TRUE),
    sum = list(fun = "colSums", is_matrix_fun = TRUE),
    list(fun = fun, is_matrix_fun = FALSE)
  )
}
