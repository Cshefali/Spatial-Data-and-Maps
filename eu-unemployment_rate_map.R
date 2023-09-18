#EU map for unemployment rates
#Last Update- Sept 18, 2023
#Author- Shefali C.

#code for unemployment rate- 'UNE_RT_M'

library(tidyverse)
library(giscoR)
library(eurostat)
library(sf)
library(scales)

lfst_r_lfu3rt

working_dir <- getwd()

#set path to cache the dataset
set_eurostat_cache_dir(cache_dir = file.path(working_dir, "data/eu-unemployment"),
                       install = T)

unemployment_data <- eurostat::get_eurostat(id = "lfst_r_lfu3rt",
                                            type = "label")

#proper NUTS-2 codes
unemployment_data2 <- eurostat::get_eurostat(id = "lfst_r_lfu3rt")

#filter data for years 2022 and 2023
data_2022_23 <- unemployment_data[grep(pattern = "2022|2023", unemployment_data$time),]

#EU Area
eu_area <- unemployment_data[grep(pattern = "Euro area", unemployment_data$geo),]
eu_area_total <- eu_area[eu_area$age == "Total",]