# TODO
# summary2():
# - work for all types and classes
# summary2_by():
# - split benchmark
# - multiple group vars
# - NA level in group

#' Alternative to `base::summary()` for data frames
#'
#' @param data A data frame
#' @param digits Number of significant digits to display for mean and quantiles
#' @return A data frame
#' @export
summary2 <- function(data, digits = 4) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  if (ncol(data) < 1) {
    return("`data` has 0 columns")
  }

  out <- vector("list", length(data))
  types <- vapply(data, FUN.VALUE = character(1), FUN = typeof)
  n <- nrow(data)

  for (i in seq_along(out)) {
    type <- types[[i]]
    vals <- data[[i]]

    if (all(is.na(vals))) {
      res <- summary_template()
      res$d_na <- 1
      out[[i]] <- res
      next
    }

    if (inherits(vals, "factor")) {
      type <- "character"
    }

    if (type %in% c("double", "integer")) {
      out[[i]] <- summary_dbl(vals)
    } else if (type == "logical") {
      out[[i]] <- summary_lgl(vals)
    } else if (type == "character") {
      out[[i]] <- summary_chr(vals)
    } else {
      out[[i]] <- summary_template()
    }
  }

  out <- data.table::rbindlist(out)
  out$name <- names(data)
  out$type <- shorten_type(types)
  out$n <- n
  out$d_na <- round(out$d_na, 4)

  for (var in c("mean", "p0", "p25", "p50", "p75", "p100")) {
    out[[var]] <- signif(out[[var]], digits = digits)
  }

  out <- out[, c("name", "type", "n", "d_na", "n_unique", "mean", "p0", "p25", "p50", "p75", "p100")]
  out
}

#' Alternative to `base::summary()` for data frames, by groups
#'
#' @param data A data frame
#' @param by A length one character vector of a variable in `data` to group by
#' @param vars Character vector of variables in `data` to keep in summary
#' @param digits Number of significant digits to display for mean and quantiles
#' @return A data frame
#' @export
summary2_by <- function(data, by, vars, digits = 4) {
  nms <- names(data)

  if (!(length(by) == 1 & by %in% nms)) {
    stop("`by` must be a single variable in `data`.", call. = FALSE)
  }
  if (!(all(vars %in% nms))) {
    stop("`vars` must be in `data`", call. = FALSE)
  }

  groups <- split(data, data[[by]])

  out <- lapply(groups, function(.x) {
    summary2(.x[vars], digits = digits)
  })

  out <- data.table::rbindlist(out)
  ord <- c(by, names(out))
  out[[by]] <- rep(names(groups), each = length(vars))
  out <- out[, ..ord]
  out
}

summary_dbl <- function(x) {
  d_na <- mean(is.na(x))
  x <- x[!is.na(x)]

  if (inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
    x <- as.numeric(x)
    alg <- 1
  } else {
    alg <- 7
  }

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
