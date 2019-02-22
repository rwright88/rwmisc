# TODO
# theme_rw() is unfinished

#' A ggplot2 theme similar to `ggplot2::theme_bw()`
#'
#' @param base_size Base font size
#' @param base_family Base font family
#' @export
theme_rw <- function(base_size = 12, base_family = "") {
  out <- ggplot2::theme_bw(base_size = base_size, base_family = base_family)
  out
}
