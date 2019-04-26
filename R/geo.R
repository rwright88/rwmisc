# TODO
# other geo levels: state, metro, census tract, etc

#' Create US metro map
#'
#' @param data Data frame containing columns cbsa_code, numeric vector fill,
#'   and optionally a numeric vector size
#' @param fill Name of numeric vector in data to fill county colors by
#' @param size Name of numeric vector in data to determine circle size
#' @export
map_us_metro <- function(data, fill, size = NULL) {
  if (!(is_installed("ggplot2") && is_installed("sf"))) {
    stop("`map_us_metro` requires ggplot2 and sf.", call. = FALSE)
  }

  stopifnot(is.data.frame(data))
  nms <- names(data)
  if (!("cbsa_code" %in% nms && fill %in% nms)) {
    stop("`data` must contain columns cbsa_code and `fill`.", call. = FALSE)
  }

  if (is.null(size)) {
    data <- data[, c("cbsa_code", fill)]
  } else {
    stopifnot(size %in% nms)
    data <- data[, c("cbsa_code", fill, size)]
    size_ <- dplyr::sym(size)
  }

  fill_ <- dplyr::sym(fill)
  data <- dplyr::left_join(us_metro_cent, data, by = "cbsa_code")

  # ~~~ temp ~~~
  data <- data[!stringr::str_detect(data$cbsa_name, ", PR|, HI|, AK"), ]
  us_state_poly <- us_state_poly[!(us_state_poly$state_fips %in% c("02", "15", "72")), ]

  if (!is.null(size)) {
    data <- data[order(data[[size]], decreasing = TRUE), ]
  }

  p <- ggplot2::ggplot(data)
  p <- p + ggplot2::geom_sf(ggplot2::aes(fill = !!fill_, size = !!size_),
    shape = 21,
    stroke = 0.1,
    show.legend = "point"
  )
  p <- p + ggplot2::geom_sf(data = us_state_poly, fill = NA, size = 0.3)
  p
}

#' Create US county map
#'
#' @param data Data frame containing columns county_fips, numeric vector fill,
#'   and optionally a numeric vector size
#' @param fill Name of numeric vector in data to fill county colors by
#' @param type polygons or centroids
#' @param size When type equals centroids, name of numeric vector in data to
#'   determine circle size
#' @param state Character vector of state FIPS codes to include in map
#' @export
map_us_county <- function(data, fill, type = c("polygons", "centroids"), size = NULL, state = NULL) {
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
    p <- p + ggplot2::geom_sf(ggplot2::aes(fill = !!fill_, size = !!size_),
      shape = 21,
      stroke = 0.1,
      show.legend = "point"
    )
  }

  p <- p + ggplot2::geom_sf(data = us_state_poly, fill = NA, size = 0.3)
  p
}
