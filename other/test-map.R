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
county <- calc_changes(county, bys = "county", first = year_first, last = year_last)

county %>%
  rename(county_fips = county) %>%
  mutate(change_aa = change_aa * 100) %>%
  mutate(change_aa = case_when(
    change_aa < -2.5 ~ -2.5,
    change_aa > 2.5 ~ 2.5,
    TRUE ~ change_aa
  )) %>%
  rwmisc::map_us_county(fill = "change_aa", type = "centroids", size = "pop") +
  # scale_fill_gradientn(colors = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_color_gradientn(colors = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_continuous(range = c(0.5, 15)) +
  rwmisc::theme_rw()
