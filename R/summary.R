# TODO:
# summary2() work for all types and classes
# summary2_by() split benchmark
# summary2_by() NA level in group
# add tests for summary2_by()

#' Alternative to summary for data frames
#'
#' @param data Data frame
#' @param probs Numeric vector of probabilities
#' @return Data frame
#' @export
summary2 <- function(data, probs = seq(0, 1, 0.25)) {
  stopifnot(is.data.frame(data))
  stopifnot(is.numeric(probs), all(probs >= 0 & probs <= 1))
  if (ncol(data) < 1) {
    return("`data` has 0 columns")
  }

  types <- vapply(data, FUN.VALUE = character(1), FUN = typeof)

  out <- lapply(data, function(vals) {
    if (all(is.na(vals))) {
      return(list(d_na = 1))
    }

    type <- typeof(vals)
    if (inherits(vals, "factor")) {
      type <- "character"
    }

    if (type %in% c("double", "integer")) {
      out <- summary_dbl(vals, probs)
    } else if (type == "logical") {
      out <- summary_lgl(vals)
    } else if (type == "character") {
      out <- summary_chr(vals)
    } else {
      out <- list(d_na = NA)
    }

    out
  })

  out <- dplyr::bind_rows(out)
  ord <- c("name", "type", "n", names(out))
  out$name <- names(data)
  out$type <- shorten_type(types)
  out$n <- nrow(data)
  out[, ord]
}

#' Alternative to summary for data frames, by groups
#'
#' @param data Data frame
#' @param by Character vector of variables in `data` to group by
#' @param vars Character vector of variables in `data` to keep in summary
#' @param probs Numeric vector of probabilities
#' @return Data frame
#' @export
summary2_by <- function(data, by, vars, probs = seq(0, 1, 0.25)) {
  stopifnot(is.data.frame(data))
  nms <- names(data)
  if (!all(by %in% nms)) {
    stop("`by` must be in `data`.", call. = FALSE)
  }
  if (!all(vars %in% nms)) {
    stop("`vars` must be in `data`.", call. = FALSE)
  }

  data <- dplyr::as_tibble(data)
  data <- data[, c(by, vars)]
  groups <- split(data, data[by], drop = TRUE)

  out <- lapply(groups, function(.x) {
    res <- summary2(.x[vars], probs)
    rows <- seq_len(nrow(res))
    by_cols <- .x[rows, by]
    c(by_cols, res)
  })

  out <- dplyr::bind_rows(out)
  out[do.call(order, out[by]), ]
}

summary_dbl <- function(x, probs = seq(0, 1, 0.25)) {
  if (inherits(x, c("numeric", "integer"))) {
    alg <- 7
  } else if (inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
    x <- as.numeric(x)
    alg <- 1
  } else {
    return(list(d_na = NA))
  }

  d_na <- mean(is.na(x))
  x <- x[!is.na(x)]
  mean1 <- mean(x)
  probs <- unique(probs)
  quantiles <- stats::quantile(x, probs = probs, names = FALSE, type = alg)
  quantiles <- stats::setNames(quantiles, paste0("p", probs * 100))

  c(
    list(d_na = d_na, mean = mean1),
    as.list(quantiles)
  )
}

summary_lgl <- function(x) {
  if (!inherits(x, "logical")) {
    return(list(d_na = NA))
  }

  d_na <- mean(is.na(x))
  n_unique <- length(unique(x))
  mean1 <- mean(x, na.rm = TRUE)

  list(
    d_na = d_na,
    n_unique = n_unique,
    mean = mean1
  )
}

summary_chr <- function(x) {
  if (!inherits(x, c("character", "factor"))) {
    return(list(d_na = NA))
  }

  d_na <- mean(is.na(x))
  n_unique <- length(unique(x))

  list(
    d_na = d_na,
    n_unique = n_unique
  )
}

shorten_type <- function(x) {
  x[x == "logical"]   <- "lgl"
  x[x == "integer"]   <- "int"
  x[x == "double"]    <- "dbl"
  x[x == "character"] <- "chr"
  x[x == "complex"]   <- "cpl"
  x
}
