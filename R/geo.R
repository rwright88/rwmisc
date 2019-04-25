# TODO
# centroid option
# other geo levels: state, metro, census tract, etc

#' Create US county choropleth map
#'
#' @param data Data frame containing columns county_fips, numeric vector fill,
#'   and optionally a numeric vector size
#' @param fill Name of numeric vector in data to fill county colors by
#' @param type polygons or centroids
#' @param size When type equals centroids, name of numeric vector in data to
#'   determine circle size
#' @param state Character vector of state FIPS codes to include in map
#' @export
map_us_county <- function(data,
                          fill,
                          type = c("polygons", "centroids"),
                          size = NULL,
                          state = NULL) {
  if (!(is_installed("ggplot2") && is_installed("sf"))) {
    stop("`map_us_county` requires ggplot2 and sf.", call. = FALSE)
  }

  stopifnot(is.data.frame(data))
  nms <- names(data)
  if (!("county_fips" %in% nms && fill %in% nms)) {
    stop("`data` must contain columns county_fips and `fill`.", call. = FALSE)
  }
  type <- match.arg(type)

  if (!is.null(size)) {
    stopifnot(type == "centroids", size %in% nms)
    size_ <- dplyr::sym(size)
  }

  if (type == "polygons") {
    counties <- us_county_poly
  } else if (type == "centroids") {
    counties <- us_county_cent
  }

  if (!is.null(state)) {
    stopifnot(is.character(state))
    us_state_poly <- us_state_poly[which(us_state_poly$state_fips %in% state), ]
    state_fips <- stringr::str_sub(counties$county_fips, 1, 2)
    counties <- counties[which(state_fips %in% state), ]
  }

  fill_ <- dplyr::sym(fill)
  counties <- dplyr::left_join(counties, data, by = "county_fips")

  if (!is.null(size)) {
    counties <- counties[order(counties[[size]], decreasing = TRUE), ]
  }

  p <- ggplot2::ggplot(counties)
  if (type == "polygons") {
    p <- p + ggplot2::geom_sf(ggplot2::aes(fill = !!fill_), color = NA)
  } else if (type == "centroids") {
    p <- p + ggplot2::geom_sf(ggplot2::aes(color = !!fill_, size = !!size_))
  }
  p <- p + ggplot2::geom_sf(data = us_state_poly, fill = NA)
  p
}
