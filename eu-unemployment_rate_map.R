#Regional Unemployment Rates across EU member states in 2022
#Original blog- https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Unemployment_statistics_at_regional_level#Regional_unemployment_rates
#Last Update- Sept 21, 2023
#Author- Shefali C.

#code for unemployment rate- 'lfst_r_lfu3rt'
#if type="labels" used, then instead of NUTS-2, country names appear.

library(tidyverse)
library(giscoR)
library(eurostat)
library(sf)
library(scales)
library(colorspace)
library(lubridate)
library(plotly)


working_dir <- getwd()
cache_directory <- file.path(working_dir, "data/eu-unemployment")

#set path to cache the dataset
set_eurostat_cache_dir(cache_dir = cache_directory, install = T,
                       overwrite = T)

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

#fetch spatial data for all NUTS 0,1,2 regions

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


data_nuts_merged <- data_2022 %>%
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

#create intervals
age_15_74$perc_range <- cut(age_15_74$values, breaks = breaks,
                            include.lowest = T)

#minor alterations in breaks to make legend labels better
#legend_labels <- prettyNum(breaks)
legend_labels <- c("< 3.1", "3.1 - <4.5", "4.5 - <6.2", "6.2 - <9.0", 
                   ">=9.0", "data not available")



#MAP

#Map of entire Europe
europe <- gisco_get_countries(year = "2020", region = "Europe")

#base map of Europe
# map_of_europe <- ggplot(data = europe)+
#                     geom_sf()+
#                     coord_sf(
#                       xlim = c(-25,45), ylim = c(30,75), expand = T
#                     )

#set a color palette
palette1 <- hcl.colors(n = length(breaks)-1, palette = "Lajolla")
palette2 <- hcl.colors(n = length(breaks)-1, palette = "heat")
palette3 <- hcl.colors(n = length(breaks)-1, palette = "ag_GrnYl")
color_palette <- c("#F0E298", "#E09D34", "#B75437", "#631C37", "#1D0B14")

scales::show_col(palette)

#country boundaries
country_lines <- nuts0 %>% st_cast("MULTILINESTRING")

#NUTS-2 boundaries
nuts2_boundary <- nuts2_crop %>% st_cast("MULTILINESTRING")
nuts2_boundary_copy <- nuts2_boundary

#remove UK and Turkey nUts data
nuts2_boundary_copy <- nuts2_boundary_copy %>% 
  filter(!CNTR_CODE %in% c("UK", "TR"))

caption_text <- paste0("Source: Eurostat, online data code- lfst_r_lfu3rt\n ", gisco_attributions())

#basic Europe map
# ggplot(data = age_15_74)+
#             geom_sf(aes(geometry = geometry, fill = perc_range))+
#             geom_sf(data = country_lines, color = "black", linewidth = 0.2)+
#             coord_sf(
#               xlim = c(-25,45), ylim = c(30,75), expand = T
#             )+
#             labs(
#               title = "EU Unemployment Rate, 2022",
#               subtitle = "(share of labour force aged 15-74 years, NUTS-2 regions)",
#               caption = caption_text
#             )+
#             scale_fill_manual(
#               name = "Unemployment avg. for EU = 6.2%",
#               values = palette1,
#               labels = legend_labels,
#               drop = F,
#               na.value = "grey",
#               guide = guide_legend(
#                         direction = "horizontal",
#                         keyheight = 0.35,
#                         keywidth = 2.50,
#                         title.position = "top",
#                         label.position = "bottom",
#                         title.hjust = 0.5,
#                         label.hjust = 0.5,
#                         nrow = 1,
#                         byrow = T
#                         #reverse = T
#               )
#             )+
#             #theme_void()+
#             #theme
#             theme(
#               #LEGEND
#               legend.title = element_text(size = 7, face = "bold", color = "black"),
#               legend.text = element_text(size = 5, face = "bold", color = "black"),
#               legend.position = "bottom",
#               #Title, subtitle &  caption
#               plot.title = element_text(size = 11, hjust = 0.5, color = "black"),
#               plot.title.position = "plot",
#               plot.subtitle = element_text(size = 10, hjust = 0.5, color = "black"),
#               plot.caption = element_text(hjust = 0.5, color = "black")
#               #background of the plot
#               #plot.background = element_rect(fill = "black")
#               
#             )


#ggsave(filename = file.path(working_dir, "images/eu-umemployment-age15-74.png"))
##Take help from- https://cran.r-project.org/web/packages/giscoR/vignettes/giscoR.html


(eu_map <- ggplot(data = europe)+
  geom_sf()+
  #creates NUTS-2 regions with given region shape
  geom_sf(data = age_15_74, aes(geometry = geometry, fill = perc_range))+
  #creates NUTS-2 boundaries
  geom_sf(data = nuts2_boundary_copy, color = "grey50",linewidth = 0.05)+
  geom_sf(data = country_lines, color = "black", linewidth = 0.4)+
  coord_sf(
    xlim = c(-25,45), ylim = c(30,75), expand = T
  )+
  labs(
    title = "EU Unemployment Rate, 2022",
    subtitle = "(share of labour force aged 15-74 years, NUTS-2 regions)",
    caption = caption_text
  )+
  scale_fill_manual(
    name = "Unemployment avg. for EU = 6.2%",
    values = palette1,
    labels = legend_labels,
    drop = F,
    na.value = "grey",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = 0.35,
      keywidth = 2.80,
      title.position = "top",
      label.position = "bottom",
      title.hjust = 0.5,
      label.hjust = 0.5,
      nrow = 1,
      byrow = T
      #reverse = T
    )
  )+
  theme_void()+
  #theme
  theme(
    #LEGEND
    legend.title = element_text(size = 7, face = "bold", color = "black"),
    legend.text = element_text(size = 5, face = "bold", color = "black"),
    legend.position = "bottom",
    #Title, subtitle &  caption
    plot.title = element_text(size = 11, hjust = 0.5, color = "black"),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "black"),
    plot.caption = element_text(hjust = 0.5, color = "black")
    #background of the plot
    #plot.background = element_rect(fill = "white"),
    #panel.background = element_rect(fill = "white"),
    #panel.grid = element_line(colour = "black", linewidth = 0.05)
    
  ))


#Adding smaller regions in a separate box
# color_palette2 <- c("[0,3.1]" = "#FCFFC9",
#                     "(3.1,4.5]" = "#E8C167",
#                   "(4.5,6.2]"  = "#D67500",
#                   "(6.2,9]" = "#913640",
#                   "(9,30]" = "#1D0B14")
# 
# #spain_nuts <- age_15_74 %>% filter(CNTR_CODE == "ES")
# 
# #spain- Canarias
# es_canarias <- ggplot(data = age_15_74 %>% filter(NAME_LATN == "Canarias"))+
#                   geom_sf(aes(geometry = geometry, fill = perc_range))+
#                   scale_fill_manual(values = color_palette2)+
#                   labs(title = "Canarias (ES)")+
#                   theme_void()+
#                   theme(
#                     legend.position = "none"
#                   )

#specific nuts from France
# france_nuts <- age_15_74 %>% 
#                 filter(CNTR_CODE == "FR",
#                        NAME_LATN %in% c('Guadeloupe',
#                                         'Martinique',
#                                         'Guyane',
#                                         'La Réunion'))
# 
# (fr_guadeloupe <- ggplot(data = france_nuts)+
#                     geom_sf(aes(geometry = geometry, fill = perc_range))+
#                     coord_sf(xlim = c(-61.24),
#                              ylim = c(16.10))+
#                     scale_fill_manual(values = color_palette2)+
#                     labs(title = "Guadeloupe (FR)")+
#                     theme_void()+
#                     theme(
#                       legend.position = "none"
#                     ))
# 
# ggplot(data = europe)+
#   geom_sf()+
#   coord_sf(
#     xlim = c(-25,45), ylim = c(30,75), expand = T
#   )