# Create sf objects

library(sf)

shp_us_state  <- "data-raw/shapefile/state/cb_2016_us_state_20m.shp"
shp_us_county <- "data-raw/shapefile/county/cb_2016_us_county_20m.shp"

create_us_state_poly <- function(file) {
  data <- sf::st_read(file, stringsAsFactors = FALSE)
  data <- setNames(data, tolower(names(data)))
  data <- data[!(data$statefp %in% c("02", "15", "72")), ]
  data <- data[, c("geoid", "name", "geometry")]
  data <- setNames(data, c("state_fips", "state_name", "geometry"))
  data <- sf::st_transform(data, crs = 102003)
  data
}

create_us_county_poly <- function(file) {
  data <- sf::st_read(file, stringsAsFactors = FALSE)
  data <- setNames(data, tolower(names(data)))
  data <- data[!(data$statefp %in% c("02", "15", "72")), ]
  data <- data[, c("geoid", "name", "geometry")]
  data <- setNames(data, c("county_fips", "county_name", "geometry"))
  data <- sf::st_transform(data, crs = 102003)
  data
}

create_us_county_cent <- function(file) {
  data <- create_us_county_poly(file)
  data <- sf::st_centroid(data)
  data
}

us_state_poly <- create_us_state_poly(shp_us_state)
us_county_poly <- create_us_county_poly(shp_us_county)
us_county_cent <- create_us_county_cent(shp_us_county)

usethis::use_data(
  us_state_poly,
  us_county_poly,
  us_county_cent,
  internal = FALSE,
  overwrite = TRUE
)
