#Create animated map of life expectancy 1990-2021.
#Data from kaggle- https://www.kaggle.com/datasets/iamsouravbanerjee/life-expectancy-at-birth-across-the-globe/code

#Progress on hold due to CRS errors.
#Last updated- Aug 22, 2023
#Author- Shefali C.

#Help blogs- 
##https://www.r-bloggers.com/2018/07/life-expectancy-animated/
##https://stackoverflow.com/questions/66317974/animated-map-using-geom-sf-and-ggplotly

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tmap)
library(knitr)
library(gganimate)
library(transformr)
#library(magick)

theme_set(theme_bw())

#Fetch spatial-data for World map from rnaturalearth package
world <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  select(iso_a3, name_long, continent)

#filter all rows from world where iso_a3 = "-99"
world_iso99 <- world %>% filter(iso_a3 == "-99")

#assign correct ISO country code
world[world$name_long == "France", 'iso_a3'] <- "FRA"
world[world$name_long == "Norway", 'iso_a3'] <- "NOR"

#read life-expectancy data (non-spatial)
data <- read_csv(paste0(getwd(), 
                        "/data/global_life_expectancy/Life Expectancy at Birth.csv"))


#create data subset
data_cropped <- data %>% 
                  select(-c(Hemisphere, `Human Development Groups`, 
                            `UNDP Developing Regions`)
                         ) %>% 
                #rename ISO3 column to match with world dataframe above
                rename(iso_a3 = ISO3)

world_spatial <- world %>% 
                  left_join(data_cropped, by = 'iso_a3')

#remove rendundant columns
world_spatial <- world_spatial %>% select(-c(Continent, Country))

#pivot the data longer
world_long <- world_spatial %>% 
                pivot_longer(cols = 5:36,
                             names_to = "year",
                             values_to = "life_exp")

ggplot(data = world_long)+
  geom_sf()

#extract year from "year" column
world_long$year <- str_extract(world_long$year, pattern = "([0-9]+)")

#convert year to integer type
world_long$year <- as.integer(world_long$year)

#World Map--this doesn't work. 
##Bug in geom_sf(). Open issue at github.
##https://github.com/thomasp85/gganimate/issues/479
ggplot(data = world_long)+
  geom_sf(aes(fill = life_exp))+
  transition_states(year)

###################################################


#Trying the same using tmap package.
##Gives error. "Invalid CRS"
tm_shape(world_long)+
  tm_fill("life_expectancy", title = "Life Expectancy", palette = "viridis")+
  tm_facets(by = "year", ncol = 3)+
  tmap_options(check.and.fix = TRUE)


##trying to make crs valid.

#returns rows in world_long that give valid crs.
t <- which(st_is_valid(world_long$geometry))

#View rows that return invalid crs.
View(world_long[-t,])

#make a copy of original df
world_long2 <- world_long

#correct the crs in rows with invalid crs.
world_long2[-t,'geometry'] <- st_make_valid(world_long2[-t,'geometry'])

##Still getting error...AAAAAaaaaaaaahhhhhhhhhhhhh
tm_shape(world_long2)+
  tm_fill("life_expectancy", title = "Life Expectancy", palette = "viridis")+
  tm_facets(by = "year", ncol = 3)+
  tmap_options(check.and.fix = TRUE)