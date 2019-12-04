#' Alternative to summary for data frames
#'
#' @param data Data frame
#' @param probs Numeric vector of probabilities
#' @param uniques Logical, count uniques?
#' @return Data frame
#' @export
summary2 <- function(data, probs = seq(0, 1, 0.25), uniques = FALSE) {
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
      out <- summary_chr(vals, uniques = uniques)
    } else {
      out <- list(d_na = NA)
    }
    out
  })

  out <- data.table::rbindlist(out, fill = TRUE)
  ord <- c("name", "type", "n", names(out))
  out$name <- names(data)
  out$type <- shorten_type(types)
  out$n <- nrow(data)
  data.table::setcolorder(out, ord)
  out
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
  quantiles <- set_names(quantiles, paste0("p", probs * 100))
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
  mean1 <- mean(x, na.rm = TRUE)
  list(d_na = d_na, mean = mean1)
}

summary_chr <- function(x, uniques = FALSE) {
  if (!inherits(x, c("character", "factor"))) {
    return(list(d_na = NA))
  }

  d_na <- mean(is.na(x))

  if (uniques == TRUE) {
    n_unique <- length(unique(x))
    list(d_na = d_na, n_unique = n_unique)
  } else {
    list(d_na = d_na)
  }
}

shorten_type <- function(x) {
  x[x == "logical"]   <- "lgl"
  x[x == "integer"]   <- "int"
  x[x == "double"]    <- "dbl"
  x[x == "character"] <- "chr"
  x[x == "complex"]   <- "cpl"
  x
}
