#Regional Unemployment Rates across EU member states in 2022
#Original blog- https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Unemployment_statistics_at_regional_level#Regional_unemployment_rates
#Last Update- Sept 19, 2023
#Author- Shefali C.

#code for unemployment rate- 'lfst_r_lfu3rt'

library(tidyverse)
library(giscoR)
library(eurostat)
library(sf)
library(scales)
library(lubridate)


working_dir <- getwd()

#set path to cache the dataset
set_eurostat_cache_dir(cache_dir = file.path(working_dir, "data/eu-unemployment"),
                       install = T)

#in this dataframe, NUTS-2 regions are represented by 'geo'
data_with_code <- eurostat::get_eurostat(id = "lfst_r_lfu3rt")

#let's see the time range of this data
summary(data_with_code$time)

unique(data_with_code$geo)

#convert time column to date format
#data_with_code$time <- as_date(data_with_code$time)


#filter data for the year 2022
data_2022 <- data_with_code %>% filter(year(time) == 2022)

#filter data with EU27_2020
eu_2020 <- data_with_code[data_with_code$geo == "EU27_2020",]