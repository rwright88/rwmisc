# TODO
# summary2() is unfinished, must work with classes such as Date, use table()?

#' Alternative to `base::summary()` for data frames
#'
#' @param data A data frame
#' @return A list
#' @export
summary2 <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }

  out <- vector("list", length(data))
  types <- lapply(data, typeof)

  for (i in seq_along(out)) {
    vals <- data[[i]]
    type <- types[[i]]
    n    <- length(vals)
    p_na <- mean(is.na(vals))
    vals <- vals[!is.na(vals)]

    if (length(vals) == 0) {
      out[[i]] <- list(n = n, p_na = p_na)
      next
    }

    if (type %in% c("double", "integer")) {

      if (inherits(vals, "Date")) {
        type <- 1
      } else {
        type <- 7
      }

      probs <- c(0, 0.25, 0.5, 0.75, 1)
      quantiles <- quantile(vals, probs = probs, na.rm = TRUE, type = type)
      res <- list(n = n, p_na = p_na, quantiles = quantiles)

    } else if (type %in% c("character", "logical")) {

      counts <- dplyr::count(dplyr::tibble(key = vals), .data$key, sort = TRUE)
      counts$d <- counts$n / sum(counts$n)
      res <- list(n = n, p_na = p_na, counts = counts)

    } else {
      res <- NA
    }

    out[[i]] <- res
  }

  names(out) <- names(data)
  out
}
