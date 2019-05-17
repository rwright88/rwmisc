# Create sf objects

library(sf)

read_shape <- function(url_zip, file) {
  temp <- tempfile()
  download.file(url = url_zip, destfile = temp)
  dir <- dirname(temp)
  unzip(temp, exdir = dir)
  file <- file.path(dir, file)
  data <- sf::st_read(file, stringsAsFactors = FALSE)
  # unlink(dir, recursive = TRUE)
  data
}

create_us_state_poly <- function() {
  data <- read_shape(
    url_zip = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
    file = "cb_2016_us_state_20m.shp"
  )
  data <- data[, c("GEOID", "NAME", "geometry")]
  data <- setNames(data, c("state_fips", "state_name", "geometry"))
  data <- sf::st_transform(data, crs = 102003)
  data
}

create_us_metro_cent <- function(file) {
  data <- read_shape(
    url_zip = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_cbsa_20m.zip",
    file = "cb_2016_us_cbsa_20m.shp"
  )
  data <- data[, c("GEOID", "NAME", "geometry")]
  data <- setNames(data, c("cbsa_code", "cbsa_name", "geometry"))
  data <- sf::st_transform(data, crs = 32617)
  data <- sf::st_centroid(data)
  data <- sf::st_transform(data, crs = 102003)
  data
}

create_us_county_poly <- function(file) {
  data <- read_shape(
    url_zip = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_20m.zip",
    file = "cb_2016_us_county_20m.shp"
  )
  data <- data[, c("GEOID", "NAME", "geometry")]
  data <- setNames(data, c("county_fips", "county_name", "geometry"))
  data <- sf::st_transform(data, crs = 102003)
  data
}

create_us_county_cent <- function(file) {
  data <- read_shape(
    url_zip = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_20m.zip",
    file = "cb_2016_us_county_20m.shp"
  )
  data <- data[, c("GEOID", "NAME", "geometry")]
  data <- setNames(data, c("county_fips", "county_name", "geometry"))
  data <- sf::st_transform(data, crs = 32617)
  data <- sf::st_centroid(data)
  data <- sf::st_transform(data, crs = 102003)
  data
}

us_state_poly <- create_us_state_poly()
us_metro_cent <- create_us_metro_cent()
us_county_poly <- create_us_county_poly()
us_county_cent <- create_us_county_cent()

usethis::use_data(
  us_state_poly,
  us_metro_cent,
  us_county_poly,
  us_county_cent,
  internal = TRUE,
  overwrite = TRUE
)
