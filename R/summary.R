# TODO
# summary2() is unfinished

#' Alternative to `base::summary()` for data frames
#'
#' @param data a data frame
#' @return list
#' @export
summary2 <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }

  out <- vector("list", length(data))
  types <- lapply(data, typeof)

  for (i in seq_along(out)) {
    var  <- data[[i]]
    type <- types[[i]]
    n    <- length(var)
    p_na <- mean(is.na(var))
    var  <- var[!is.na(var)]

    if (length(var) == 0) {
      out[[i]] <- NA
      next
    }

    if (type == "double") {

      probs <- c(0, 0.25, 0.5, 0.75, 1)
      quantiles <- quantile(var, probs = probs, na.rm = TRUE)
      res <- list(n = n, p_na = p_na, quantiles = quantiles)

    } else if (type %in% c("character", "logical")) {

      counts <- vctrs::vec_count(var, sort = "count")
      counts$p <- counts$count / sum(counts$count)
      res <- list(n = n, p_na = p_na, counts = dplyr::as_tibble(counts))

    } else if (type == "integer") {

      counts <- vctrs::vec_count(var, sort = "key")
      counts$p <- counts$count / sum(counts$count)
      counts$p_cume <- cumsum(counts$p)
      res <- list(n = n, p_na = p_na, counts = dplyr::as_tibble(counts))

    } else {
      res <- NA
    }

    out[[i]] <- res
  }

  names(out) <- names(data)
  out
}
