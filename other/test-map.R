# US county/metro pop change

library(tidyverse)
library(popest)
library(rwmisc)

year_first <- 2010
year_last  <- 2017

# funs --------------------------------------------------------------------

get_county <- function(last, file = NULL) {
  if (is.null(file)) {
    data <- popest::read_county(last)
  } else {
    data <- read_csv(file)
  }

  data <- popest::clean_county(data, last)
  data <- group_by(data, year, county)
  data <- summarise(data, pop = sum(pop))
  data <- ungroup(data)
  data
}

get_metro <- function(data) {
  data <- left_join(data, popest::cw_metro, by = c("county" = "county_fips"))
  data <- group_by(data, year, cbsa_code, cbsa_name)
  data <- summarise(data, pop = sum(pop))
  data <- ungroup(data)
  data
}

calc_changes <- function(data, bys, first, last) {
  bys_ <- syms(bys)
  n <- last - first
  data <- data[data$year %in% c(first, last), ]
  data <- data[order(data$year), ]
  data <- group_by(data, !!!bys_)
  data <- mutate(data, change_aa = (pop / pop[1]) ^ (1 / !!n) - 1)
  data <- ungroup(data)
  data <- data[data$year == last, ]
  data
}

# run ---------------------------------------------------------------------

county <- get_county(last = year_last, file = "~/data/popest/cc-est2017-alldata.csv")
metro <- get_metro(county)

county <- calc_changes(county, bys = "county", first = year_first, last = year_last)
metro <- calc_changes(metro, bys = c("cbsa_code", "cbsa_name"), first = year_first, last = year_last)

states <- unique(str_sub(county$county, 1, 2))
states <- states[!(states %in% c("02", "15", "72"))]

county %>%
  rename(county_fips = county) %>%
  mutate(change_aa = change_aa * 100) %>%
  mutate(change_aa = case_when(
    change_aa < -2 ~ -2,
    change_aa > 2 ~ 2,
    TRUE ~ change_aa
  )) %>%
  rwmisc::map_us_county(fill = "change_aa", type = "centroids", size = "pop", state = states) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "RdBu")) +
  scale_size_continuous(range = c(0.5, 15), guide = FALSE) +
  rwmisc::theme_rw()

ggsave("~/map-county.png", dpi = 300, width = 14, height = 8)

metro %>%
  mutate(change_aa = change_aa * 100) %>%
  mutate(change_aa = case_when(
    change_aa < -2 ~ -2,
    change_aa > 2 ~ 2,
    TRUE ~ change_aa
  )) %>%
  rwmisc::map_us_metro(fill = "change_aa", size = "pop") +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "RdBu")) +
  scale_size_continuous(range = c(0.5, 20), guide = FALSE) +
  rwmisc::theme_rw()

ggsave("~/map-metro.png", dpi = 300, width = 14, height = 8)
