# TODO
# summary2() is unfinished:
# - how many sigfig digits?
# - other types like list, complex, etc.
# - classes such as Date, factor, etc.

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
  out <- setNames(out, names(data))
  types <- vapply(data, FUN.VALUE = character(1), FUN = typeof)
  n <- nrow(data)

  template <- list(
    d_na = NA,
    n_unique = NA,
    mean = NA,
    p0 = NA,
    p25 = NA,
    p50 = NA,
    p75 = NA,
    p100 = NA
  )

  for (i in seq_along(out)) {
    type <- types[[i]]
    big4 <- c("logical", "integer", "double", "character")

    if (!(type %in% big4)) {
      out[[i]] <- template
      next
    }

    vals <- data[[i]]
    d_na <- mean(is.na(vals))
    vals <- vals[!is.na(vals)]

    if (length(vals) == 0) {
      res <- template
      res$d_na <- d_na
      out[[i]] <- res
      next
    }

    if (inherits(vals, "factor")) {
      type <- "character"
    }

    if (type %in% c("double", "integer")) {

      if (inherits(vals, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
        vals <- as.numeric(vals)
        alg <- 1
      } else {
        alg <- 7
      }

      mean1 <- mean(vals)
      probs <- c(0, 0.25, 0.5, 0.75, 1)
      quantiles <- quantile(vals, probs = probs, na.rm = TRUE, type = alg)

      out[[i]] <- list(
        d_na = d_na,
        n_unique = NA,
        mean = mean1,
        p0 = quantiles[1],
        p25 = quantiles[2],
        p50 = quantiles[3],
        p75 = quantiles[4],
        p100 = quantiles[5]
      )

    } else if (type == "logical") {

      n_unique <- length(unique(vals))
      mean1 <- mean(vals)

      out[[i]] <- list(
        d_na = d_na,
        n_unique = n_unique,
        mean = mean1,
        p0 = NA,
        p25 = NA,
        p50 = NA,
        p75 = NA,
        p100 = NA
      )

    } else if (type == "character") {

      n_unique <- length(unique(vals))

      out[[i]] <- list(
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
  }

  types[types == "logical"]   <- "lgl"
  types[types == "integer"]   <- "int"
  types[types == "double"]    <- "dbl"
  types[types == "character"] <- "chr"
  types[types == "complex"]   <- "cpl"

  out <- data.table::rbindlist(out)
  out$name <- names(data)
  out$type <- types
  out$n <- n
  out$d_na <- round(out$d_na, 4)

  for (var in c("mean", "p0", "p25", "p50", "p75", "p100")) {
    out[[var]] <- signif(out[[var]], digits = digits)
  }

  out <- out[, c("name", "type", "n", "d_na", "n_unique", "mean", "p0", "p25", "p50", "p75", "p100")]
  out
}

#' Summary of data frame by a variable
#'
#' @param data A data frame
#' @param by A length one character vector of a variable in `data` to group by
#' @param vars Character vector of variables in `data` to keep in summary
#' @param digits Number of significant digits to display for mean and quantiles
#' @return A data frame
#' @export
summary2_by <- function(data, by, vars, digits = 4) {
  names1 <- names(data)

  if (!(length(by) == 1 & by %in% names1)) {
    stop("`by` must be a single variable in `data`.", call. = FALSE)
  }
  if (!(all(vars %in% names1))) {
    stop("`vars` must be in `data`", call. = FALSE)
  }

  groups <- split(data, data[[by]])

  out <- lapply(groups, function(.x) {
    summary2(.x[vars], digits = digits)
  })

  out <- data.table::rbindlist(out)
  out[[by]] <- rep(names(groups), each = length(vars))
  out
}
