# Create sf objects

library(sf)

shp_us_state  <- "data-raw/shapefile/state/cb_2016_us_state_20m.shp"
shp_us_metro  <- "data-raw/shapefile/metro/cb_2016_us_cbsa_20m.shp"
shp_us_county <- "data-raw/shapefile/county/cb_2016_us_county_20m.shp"

create_us_state_poly <- function(file) {
  data <- sf::st_read(file, stringsAsFactors = FALSE)
  data <- data[, c("GEOID", "NAME", "geometry")]
  data <- setNames(data, c("state_fips", "state_name", "geometry"))
  data <- sf::st_transform(data, crs = 102003)
  data
}

create_us_metro_cent <- function(file) {
  data <- sf::st_read(file, stringsAsFactors = FALSE)
  data <- data[, c("GEOID", "NAME", "geometry")]
  data <- setNames(data, c("cbsa_code", "cbsa_name", "geometry"))
  data <- sf::st_transform(data, crs = 32617)
  data <- sf::st_centroid(data)
  data <- sf::st_transform(data, crs = 102003)
  data
}

create_us_county_poly <- function(file) {
  data <- sf::st_read(file, stringsAsFactors = FALSE)
  data <- data[, c("GEOID", "NAME", "geometry")]
  data <- setNames(data, c("county_fips", "county_name", "geometry"))
  data <- sf::st_transform(data, crs = 102003)
  data
}

create_us_county_cent <- function(file) {
  data <- sf::st_read(file, stringsAsFactors = FALSE)
  data <- data[, c("GEOID", "NAME", "geometry")]
  data <- setNames(data, c("county_fips", "county_name", "geometry"))
  data <- sf::st_transform(data, crs = 32617)
  data <- sf::st_centroid(data)
  data <- sf::st_transform(data, crs = 102003)
  data
}

us_state_poly <- create_us_state_poly(shp_us_state)
us_metro_cent <- create_us_metro_cent(shp_us_metro)
us_county_poly <- create_us_county_poly(shp_us_county)
us_county_cent <- create_us_county_cent(shp_us_county)

usethis::use_data(
  us_state_poly,
  us_metro_cent,
  us_county_poly,
  us_county_cent,
  internal = FALSE,
  overwrite = TRUE
)
