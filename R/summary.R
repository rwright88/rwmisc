# TODO
# summary2() work for all types and classes
# summary2_by() split benchmark
# summary2_by() multiple group vars
# summary2_by() NA level in group

#' Alternative to summary for data frames
#'
#' @param data A data frame
#' @return A data frame
#' @export
summary2 <- function(data) {
  stopifnot(is.data.frame(data))

  if (ncol(data) < 1) {
    return("`data` has 0 columns")
  }

  types <- vapply(data, FUN.VALUE = character(1), FUN = typeof)
  n <- nrow(data)

  out <- lapply(data, function(vals) {
    type <- typeof(vals)

    if (all(is.na(vals))) {
      out <- summary_template()
      out$d_na <- 1
      return(out)
    }

    if (inherits(vals, "factor")) {
      type <- "character"
    }

    if (type %in% c("double", "integer")) {
      summary_dbl(vals)
    } else if (type == "logical") {
      summary_lgl(vals)
    } else if (type == "character") {
      summary_chr(vals)
    } else {
      summary_template()
    }
  })

  out <- dplyr::bind_rows(out)
  out$name <- names(data)
  out$type <- shorten_type(types)
  out$n <- n
  ord <- c("name", "type", "n", "d_na", "n_unique", "mean", "p0", "p25", "p50", "p75", "p100")
  out <- out[, ord]
  out
}

#' Alternative to summary for data frames, by groups
#'
#' @param data A data frame
#' @param by A character vector of variables in `data` to group by
#' @param vars Character vector of variables in `data` to keep in summary
#' @return A data frame
#' @export
summary2_by <- function(data, by, vars) {
  nms <- names(data)
  stopifnot(is.data.frame(data))
  stopifnot(all(by %in% nms))
  stopifnot(all(vars %in% nms))

  data <- dplyr::as_tibble(data)
  data <- data[, c(by, vars)]
  groups <- split(data, data[by], drop = TRUE)

  out <- lapply(groups, function(x) {
    res <- summary2(x[vars])
    res <- dplyr::bind_cols(x[seq_len(nrow(res)), by], res)
  })

  out <- dplyr::bind_rows(out)
  out <- out[do.call(order, out[by]), ]
  out
}

summary_dbl <- function(x) {
  if (inherits(x, c("numeric", "integer"))) {
    alg <- 7
  } else if (inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
    x <- as.numeric(x)
    alg <- 1
  } else {
    return(summary_template())
  }

  d_na <- mean(is.na(x))
  x <- x[!is.na(x)]
  mean1 <- mean(x)
  probs <- c(0, 0.25, 0.5, 0.75, 1)
  quantiles <- quantile(x, probs = probs, na.rm = TRUE, type = alg)

  list(
    d_na = d_na,
    n_unique = NA,
    mean = mean1,
    p0 = quantiles[1],
    p25 = quantiles[2],
    p50 = quantiles[3],
    p75 = quantiles[4],
    p100 = quantiles[5]
  )
}

summary_lgl <- function(x) {
  if (!inherits(x, "logical")) {
    return(summary_template())
  }

  d_na <- mean(is.na(x))
  x <- x[!is.na(x)]
  n_unique <- length(unique(x))
  mean1 <- mean(x)

  list(
    d_na = d_na,
    n_unique = n_unique,
    mean = mean1,
    p0 = NA,
    p25 = NA,
    p50 = NA,
    p75 = NA,
    p100 = NA
  )
}

summary_chr <- function(x) {
  if (!inherits(x, c("character", "factor"))) {
    return(summary_template())
  }

  d_na <- mean(is.na(x))
  x <- x[!is.na(x)]
  n_unique <- length(unique(x))

  list(
    d_na = d_na,
    n_unique = n_unique,
    mean = NA,
    p0 = NA,
    p25 = NA,
    p50 = NA,
    p75 = NA,
    p100 = NA
  )
}

summary_template <- function() {
  list(
    d_na = NA,
    n_unique = NA,
    mean = NA,
    p0 = NA,
    p25 = NA,
    p50 = NA,
    p75 = NA,
    p100 = NA
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
