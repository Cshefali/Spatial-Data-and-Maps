##Distribution Maps of Asylum seekers in EU Member nations.
##Last Updated- Sept 11, 2023
##Author- Shefali C.
##Data source- https://ec.europa.eu/eurostat/data/database

library(tidyverse)
library(giscoR)
library(eurostat)
library(ggthemes)
library(sf)
library(scales) #show_col() used to see color palettes.

##Decisions granting temporary protection by citizenship, age and sex – monthly data

#asylum seekers with temporary protection.
asylum_data <- get_eurostat(id = "migr_asytpfm")

#Andorra data
andorra <- asylum_data %>% filter(citizen == "AD")
ukraine <- asylum_data %>% filter(citizen == "UA")
#Subset data where Age = "total". 
#Implies total count of migrants of all age groups. 
total_pop <- asylum_data %>% filter(age == "TOTAL")

#adding country names along with country codes

#fetch all country details from giscoR
country_codes <- gisco_countrycode %>% 
  select(iso.name.en, CNTR_CODE, ISO3_CODE, continent, eu)

#add the country name columns in total_pop dataframe
total_pop <- total_pop %>% 
              left_join(country_codes %>% select(CNTR_CODE, iso.name.en),
                        by = c("citizen" = "CNTR_CODE")) %>% 
              #remove the 'unit' column. 
              select(-unit)

#ukraine data
ukraine_asylum <- asylum_data %>% filter(citizen == "UA")


citizen_countries <- asylum_data %>% 
                      inner_join(country_codes, by = c("citizen" = "CNTR_CODE"))

citizen_countries <- country_codes %>% 
                      left_join(asylum_data, by = c("CNTR_CODE" = "citizen")) %>% 
                      select(iso.name.en, CNTR_CODE, ISO3_CODE) %>% 
                      distinct()

citizen_countries <- country_codes %>% 
                      inner_join(asylum_data, by = c("CNTR_CODE" = "citizen")) %>% 
                      select(iso.name.en, CNTR_CODE, ISO3_CODE, time, values) %>% 
                      distinct()