##Distribution Maps of Asylum seekers in EU Member nations.
##Last Updated- Sept 12, 2023
##Author- Shefali C.
##Data source- https://ec.europa.eu/eurostat/data/database

library(tidyverse)
library(giscoR)
library(eurostat)
library(ggthemes)
library(sf)
library(scales) #show_col() used to see color palettes.

working_dir <- getwd()
##Decisions granting temporary protection by citizenship, age and sex – monthly data

#asylum seekers with temporary protection.
asylum_data <- get_eurostat(id = "migr_asytpfm", cache = T,
                            cache_dir = paste0(working_dir,"data/asylum_seekers_eurostat"))


#Subset data where Age = "total". 
#Implies total count of migrants of all age groups. 
total_pop <- asylum_data %>% filter(age == "TOTAL") %>% select(-unit)

#remove rows with following values in 'geo'
total_pop <- total_pop %>% 
              filter(!citizen %in% c("EU27_2020", "EXT_EU27_2020", "RNC", 
                                    "STLS", "TOTAL", "UK_OCT", "UNK", "XK"))

#remove "EU27_2020" from 'geo' column
total_pop <- total_pop %>% filter(!geo == "EU27_2020")

#adding country names along with country codes

#fetch all country details from giscoR
country_codes <- gisco_countrycode %>% 
  select(iso.name.en, CNTR_CODE, ISO3_CODE, continent, eu)

#match country code in 'citizen' with country names and add the
#asylum seekers nationality
total_pop2 <- total_pop %>% 
              left_join(country_codes %>% select(CNTR_CODE, iso.name.en),
                        by = c("citizen" = "CNTR_CODE")) %>% 
              rename(seeker_nationality = iso.name.en)


#match country code in 'geo' with country names and add the
#destination country's name in new column
total_pop2 <- total_pop2 %>% 
                left_join(country_codes %>% select(CNTR_CODE, iso.name.en),
                          by = c("geo" = "CNTR_CODE")) %>% 
                rename(destination = iso.name.en)

#group by each country to see how many refugees took shelter in each country
#per month 
total_pop_by_country <- total_pop2 %>% 
  group_by(seeker_nationality, time, geo,destination) %>% 
  summarize(total_count = sum(values))


#top 5 countries with maximum refugees in EU between Mar 2022- Aug 2023
top_seeker_nations <- total_pop_by_country %>% 
                        group_by(seeker_nationality) %>% 
                        summarize(total_count = sum(total_count)) %>% 
                        arrange(-total_count) %>% 
                        head(5)


#ukraine data
ukraine <- total_pop_by_country %>% filter(seeker_nationality == "Ukraine")

#total ukrainian citizens in EU states in 1 year
ukraine_aggregate <- ukraine %>% 
                      group_by(geo, destination) %>% 
                      summarise(total = sum(total_count))

#EU map using giscoR
