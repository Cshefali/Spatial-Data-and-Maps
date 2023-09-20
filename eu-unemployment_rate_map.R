#Regional Unemployment Rates across EU member states in 2022
#Original blog- https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Unemployment_statistics_at_regional_level#Regional_unemployment_rates
#Last Update- Sept 20, 2023
#Author- Shefali C.

#code for unemployment rate- 'lfst_r_lfu3rt'
#if type="labels" used, then instead of NUTS-2, country names appear.

library(tidyverse)
library(giscoR)
library(eurostat)
library(sf)
library(scales)
library(lubridate)


working_dir <- getwd()
cache_directory <- file.path(working_dir, "data/eu-unemployment")

#set path to cache the dataset
set_eurostat_cache_dir(cache_dir = cache_directory, install = T)

#in this dataframe, NUTS-2 regions are represented by 'geo'
#data_with_code <- eurostat::get_eurostat(id = "lfst_r_lfu3rt", cache = T)

#since cache_dir not working, saving this object explicitly
#write_rds(data_with_code, file = paste0(cache_directory, "/lfst_r_lfu3rt_code.rds"))

#read the downloaded RDS file
data_with_labels <- readRDS(file = paste0(cache_directory, "/UNE_RT_M_date_label_FF.rds"))

#read the data_with_code from cache directory
data_with_code <- read_rds(file = paste0(cache_directory, "/lfst_r_lfu3rt_code.rds"))
#make a copy
data_with_code2 <- data_with_code

#let's see the time range of this data
summary(data_with_code$time)


#convert time column to date format
data_with_code2$time <- as.POSIXlt(data_with_code$time)


#filter data for the year 2022
data_2022 <- data_with_code2 %>% filter(year(time) == 2022)

#filter data with EU27_2020
geo_eu_2020 <- data_with_code2[data_with_code2$geo == "EU27_2020",]
geo_ea20 <- data_with_code[data_with_code2$geo == "EA20",]

#'isced11' refers to level of education.
#Refer- https://dd.eionet.europa.eu/vocabulary/eurostat/isced11/view
#UNK is unknown, NRP is No Response, TOTAL means All ISCED 2011 levels.

#filter out data with "TOTAL" education level

data_2022 <- data_2022 %>% filter(isced11 == "TOTAL", sex == "T")
#remove ""EU27_2020" from geo column
data_2022 <- data_2022 %>% filter(!geo %in% c("EU27_2020", "EA20"))

#fetch spatial data for all NUTS 0,12 regions

nuts0 <- gisco_get_nuts(year = "2021", cache = T, nuts_level = 0,
                        resolution = "20")
nuts1 <- gisco_get_nuts(year = "2021", nuts_level = 1, resolution = "20",
                        cache = T)
nuts2 <- gisco_get_nuts(year = "2021", cache = T, resolution = "20",
                        nuts_level = 2)

#create a subset of the NUTS-2 df
nuts0_crop <- nuts0 %>% select(NUTS_ID,LEVL_CODE,CNTR_CODE,NAME_LATN, geometry)
nuts1_crop <- nuts1 %>% select(NUTS_ID,LEVL_CODE,CNTR_CODE,NAME_LATN, geometry)
nuts2_crop <- nuts2 %>% select(NUTS_ID, LEVL_CODE,CNTR_CODE,NAME_LATN, geometry)

all_nuts <- dplyr::bind_rows(nuts0_crop, nuts1_crop, nuts2_crop) %>% arrange(NUTS_ID)


data_merged1 <- data_2022 %>%
                  select(age, geo, values) %>% 
                  left_join(all_nuts,
                            by = c("geo" = "NUTS_ID"))

country_level_data <- data_2022 %>% 
                        select(age, geo, values) %>% 
                        inner_join(nuts0_crop,
                                  by = c("geo" = "NUTS_ID"))

state_level_data <- data_2022 %>% 
                      select(age, geo, values) %>% 
                      inner_join(nuts1_crop,
                                 by = c("geo" = "NUTS_ID"))

district_level_data <- data_2022 %>% 
                          select(age, geo, values) %>% 
                          inner_join(nuts2_crop,
                                     by = c("geo" = "NUTS_ID"))


#age = Y15-74
age_15_74 <- district_level_data %>% filter(age == "Y15-74") %>% select(-age)
#age = Y15-29
age_15_29 <- district_level_data %>% filter(age == "Y15-29") %>% select(-age)


#In order to create intervals for map colors, umemployment percentage
#is being categorized into different intervals.

#check the range of percentage values
summary(age_15_74$values)

#create breaks
breaks <- c(0, 3.1, 4.5, 6.2, 9.0, ceiling(max(age_15_74$values, na.rm = T))+1)

#trying breaks on a random sample

temp <- age_15_74$values[sample(nrow(age_15_74), 50)]
cut(temp, breaks = breaks, include.lowest = T)

#create intervals
age_15_74$perc_range <- cut(age_15_74$values, breaks = breaks,
                            include.lowest = T)

#minor alterations in breaks to make legend labels better
legend_labels <- prettyNum(breaks)
legend_labels[1] <- "<3.1"
#legend_labels[4] <- 
#MAP


#basic Europe map
ggplot(data = age_15_74)+
  geom_sf(aes(geometry = geometry))