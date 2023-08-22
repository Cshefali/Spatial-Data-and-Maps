library(wbstats) # the World Bank data downloading
library(tidyverse)
library(sf) # spatial data manipulation
library(rnaturalearth) # access to spatial data
library(tmap) # spatial data visualization

wb_data_create = function(indicator, new_name, year, ...){
  df = wb_data(indicator = indicator,
          start_date = year, end_date = year, ...) %>%
    select(iso_a2 = iso2c, value, year = date) %>%
    mutate(indicator = new_name) %>%
    spread(indicator, value) %>%
    as.data.frame()
  # return the dataframe
  df
}

data_life_exp = seq(1963, 2013, by = 5) %>%
  map_dfr(wb_data_create,
          indicator = "SP.DYN.LE00.IN",
          new_name = "life_exp",
          country = "countries_only")

df = wb_data(indicator = indicator,
             start_date = year, end_date = year, ...)