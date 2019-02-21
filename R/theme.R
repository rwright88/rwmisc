#' A ggplot2 theme similar to `ggplot2::theme_bw()`
#'
#' @param base_size base font size
#' @param base_family base font family
#' @export
theme_rw <- function(base_size = 12, base_family = "") {
  out <- ggplot2::theme_bw(base_size = base_size, base_family = base_family)
  out
}
