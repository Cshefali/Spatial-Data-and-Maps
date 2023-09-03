#Map of Netherlands using tmap package
#Author- Shefali C.
#Date- June 6, 2023

#load libraries
library(tidyverse)
library(tmap)
library(sf)

#load required built-in datasets
data("NLD_muni", "NLD_prov", World, metro, land, rivers)

#checking CRS format of all dataframes
str(attr(World$geometry, "crs"))
str(attr(metro$geometry, "crs"))
str(attr(rivers$geometry, "crs"))
str(attr(NLD_muni$geometry, "crs"))
str(attr(NLD_prov$geometry, "crs"))

#changing CRS format from "proj4string" to "WKT" for metro and NLD dfs
sf::st_crs(metro) <- 4326
sf::st_crs(NLD_muni) <- 4326
sf::st_crs(NLD_prov) <- 4326

#Filter out Europe data
europe <- World %>% filter(continent == "Europe")

#plotting Europe df as it is shows a wide gap between mainland and an island
tm_shape(europe)+
  tm_polygons(col = "cornsilk")

#crop extent of Europe to focus on mailand
europe_cropped <- sf::st_crop(europe,
                              #longitude extent
                              xmin = -20, xmax  = 45,
                              #latitude extent
                              ymin = 30, ymax = 75)

#Map looks better now
tm_shape(europe_cropped)+
  tm_polygons(col = "cornsilk")+
  #n.x implies number of longitudes lines to be displayed
  #n.y implies number of latitude lines to be displayed
  #col = "white" changes grid color to white, hence transparent. Not done here.
  tm_graticules(n.x = 2, n.y = 4)

##MAP 1: 

#highlight Netherlands in Europe Map
#name column in df is of factor type, level of Netherlands is 112

#creating color palette for all 177 countries
country_colors <- c(rep("cornsilk",112),"brown",rep("cornsilk",64))

tm_shape(europe_cropped)+
  tm_polygons(col = "name", 
              palette = country_colors)+
  tm_graticules(n.x = 2, n.y = 4)+
  tm_layout(legend.show = F, tmap_options(max.categories = 177))+
  tm_credits(text = "Source: Statistics Netherlands (CBS) and Kadaster Nederland (2013)",
             position = c(0,0), size = 0.4)

##MAP 2:

tmap_options(check.and.fix = T)

tm_shape(NLD_muni)+
  tm_polygons(col = "cornsilk")

tm_shape(NLD_muni) +
  tm_fill(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"),
          style="kmeans", 
          palette=list("Oranges", "Greens", "Blues", "Purples", "Greys"),
          title=c("Population 0 to 14", "Population 15 to 24", "Population 25 to 44",
                  "Population 45 to 64", "Population 65 and older"))

#NETHERLANDS MAPS THROUGH FACETS
tm_shape(NLD_muni) +
  tm_fill(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"),
          style="kmeans", 
          palette=list("Oranges", "Greens", "Blues", "Purples", "Greys"),
          title=c("Population 0 to 14", "Population 15 to 24", "Population 25 to 44",
                  "Population 45 to 64", "Population 65 and older")) +
  tm_shape(NLD_prov) +
  tm_borders() +
  tm_format("NLD", frame = TRUE, asp=0)

# CASE 3: Facets defined by group-by variable(s)
# A group-by variable that divides the objects spatially
tm_shape(NLD_prov) +
  tm_polygons("gold2") +
  tm_facets(by="name")

#Netherlands municipalities in each province 
tm_shape(NLD_muni) +
  tm_borders() +
  tm_facets(by="province") +
  tm_fill("population", style="kmeans", convert2density = TRUE) +
  tm_shape(NLD_prov) +
  tm_borders(lwd=4) +
  tm_facets(by="name")

#######---taken from text annotation---
#URL- https://search.r-project.org/CRAN/refmans/tmap/html/tm_credits.html
tm_shape(NLD_muni) +
  tm_fill(col="population", convert2density=TRUE, 
          style="kmeans", title = expression("Population (per " * km^2 * ")")) +
  tm_borders("grey25", alpha=.5) + 
  tm_shape(NLD_prov) +
  tm_borders("grey40", lwd=2) +
  tm_format("NLD", bg.color="white", frame = TRUE) +
  tm_credits("(c) Statistics Netherlands (CBS) and\nKadaster Nederland", 
             position=c("left", "bottom"))