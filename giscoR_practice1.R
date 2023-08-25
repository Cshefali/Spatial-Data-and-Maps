#GiscoR maps practice
#Author- Shefali C.
#Last updated- Aug 18, 2023

#Source- https://ropengov.github.io/giscoR/


library(tidyverse)
library(sf)
library(giscoR)

##GERMANY

#germany at resolution level 60M
DEU_res60 <- giscoR::gisco_get_countries(country = "DEU", resolution = "60") %>% 
                        mutate(res = "60m")

#germany at resolution 20M
DEU_res20 <- giscoR::gisco_get_countries(country = "DEU", resolution = "20") %>% 
                        mutate(res = "20m")

#germany at resolution 3M
DEU_res3 <- giscoR::gisco_get_countries(country = "DEU", resolution = "03") %>% 
                        mutate(res = "3m")

#germany at resolution 1M
DEU_res1 <- giscoR::gisco_get_countries(country = "DEU", resolution = "01") %>% 
                        mutate(res = "1m")

#combine data for all resolutions
germany <- bind_rows(DEU_res60, DEU_res20, DEU_res3, DEU_res1)

#Germany maps of all resolutions.
ggplot(germany)+
  geom_sf(fill = "tomato")+
  facet_wrap(~res)+
  theme_minimal()

##Map for India
IND_res1 <- giscoR::gisco_get_countries(country = "IND", resolution = "01")

#map
ggplot(IND_res1)+
  geom_sf(fill = "lightblue")+
  theme_minimal()

#Map for Pakistan
PAK_res1 <- giscoR::gisco_get_countries(country = "PAK", resolution = "01")

ggplot(PAK_res1)+
  geom_sf(fill = "lightgreen")+
  theme_minimal()

#Map for China
CHN_res1 <- giscoR::gisco_get_countries(country = "CHN", resolution = "01")

ggplot(CHN_res1)+
  geom_sf(fill = "red")+
  theme_minimal()

#bind India, Pakistan & China
IND_PAK_CHN <- bind_rows(IND_res1, PAK_res1, CHN_res1)

ggplot(IND_PAK_CHN)+
  geom_sf(fill = c("IND" = "lightblue", "PAK" = "lightgreen", "CHN" = "red"))+
  theme_minimal()

#United Kingdom
UK_res1 <- giscoR::gisco_get_countries(country = "UK", res = "01")

ggplot(UK_res1)+
  geom_sf(fill = "gold")+
  theme_minimal()

##Map with line shapefiles

#POINT shapefiles

africa_labs <- giscoR::gisco_get_countries(
  spatialtype = "LB",
  #Web Mercartor projection, used by Google Maps, OpenStreetMaps etc.
  epsg = "3857",
  region = "Africa"
)

#get african coastline LINE shapefile
africa_coast <- giscoR::gisco_get_countries(
  spatialtype = "COASTL",
  epsg = "3857"
)

#gives minimum and maximum coordinates for African continent.
#This basically zooms in the otherwise world map to focus on the Africa.
africa_bbox <- sf::st_bbox(africa_labs)

#Map of African coast
ggplot(africa_coast)+
  #aesthetic "color" used & not "fill" because coastlines are line shapefiles.
  geom_sf(colour = "deepskyblue4", linewidth = 2)+
  geom_sf(data = africa_labs, fill = "springgreen4", colour = "darkgoldenrod1", shape = 21, size = 3)+
  coord_sf(xlim = africa_bbox[c("xmin", "xmax")],
           ylim = africa_bbox[c("ymin", "ymax")])+
  theme_minimal()

#------------------------------------------------
all_coast <- giscoR::gisco_get_countries(
  spatialtype = "COASTL",
  #WSG 84 projection used in GPS tech.
  epsg = "4326"
)

ggplot(all_coast)+
  geom_sf(color = "red", linewidth = 1)+
  theme_minimal()
#---------------------------------------------------


#Map with Labels- European NUTS levels used.

#get NUTS level 1 region names for Italy
italy_nuts1 <- giscoR::gisco_get_nuts(country = "Italy", nuts_level = 1)

#labelled map
ggplot(italy_nuts1)+
  geom_sf(aes(fill = NAME_LATN), color = "black")+
  geom_sf_text(aes(label = NAME_LATN), size = 4)+
  theme_minimal()+
  theme(axis.title = element_blank())
  
##--------------------------GISCO_COASTLINE()-----------------
#World coastlines
world_coasts <- gisco_coastallines


#Zoom in on Oceania coastlines
ggplot(world_coasts)+
  geom_sf(color = "cyan", fill = "cyan", alpha = 0.2)+
  #zoom on Oceania part
  coord_sf(xlim = c(96, 179),
           ylim = c(-51, 11)
           )+
  theme_minimal()+
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "grey90")
  )


##-------------------------GISCO_COUNTRIES()-----------------
countries <- gisco_countries

#shapefile for Brazil
brazil = subset(countries, ISO3_CODE == "BRA")

ggplot(data = brazil)+
  geom_sf(fill = "springgreen4", color = "darkgoldenrod1", linewidth = 2)+
  theme_void()

#this functions get the list of entire content available with GiscoR
data("gisco_db")

##---------ARIRPORTS---------------
greece <- gisco_get_countries(country = "EL", res = "1")
#airports of greece
greece_airport <- gisco_get_airports(country = "EL")
#transform the CRS
greece_airport <- st_transform(greece_airport, st_crs(greece))

#map
ggplot(greece)+
  geom_sf(fill = "grey80")+
  geom_sf(data = greece_airport, color = "blue")+
  labs(title = "Airports of Greece",
       caption = gisco_attributions())+
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 0))

##-------------SEAPORTS------------
world_ports <- gisco_get_ports(year = "2013")
world_coasts <- gisco_get_coastallines(year = "2013")

#For equal Earth projection
world_coasts <- st_transform(world_coasts, 8857)
world_ports <- st_transform(world_ports, st_crs(world_coasts))

ggplot(data = world_coasts)+
  geom_sf(fill = "#F6E1B9", color = "#0978AB")+
  geom_sf(data = world_ports, fill = "red", shape = 21)+
  #theme_void()+
  theme(
    panel.background = element_rect(fill = "#C6ECFF"),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5)
  ) +
  labs(
    title = "Ports Worldwide", subtitle = "Year 2013",
    caption = "(c) European Union, 1995 - today"
  )

##-------------COASTLINES---------
coast <- gisco_get_coastallines()

ggplot(coast) +
  geom_sf(color = "#1278AB", fill = "#FDFBEA") +
  # Zoom on Caribe
  coord_sf(
    xlim = c(-99, -49),
    ylim = c(4, 30)
  ) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#C7E7FB", color = "black"))

##------------

##using gisco_get_grid()
#URL- https://rdrr.io/cran/giscoR/man/gisco_get_grid.html

grid <- gisco_get_grid(resolution = "20")

#For details about what all columns represent, refer PDF link below
#https://gisco-services.ec.europa.eu/grid/GISCO_grid_metadata.pdf


#data for 2021
grid_2021 <- grid %>% 
  select(DIST_BORD, GRD_ID, X_LLC, Y_LLC, CNTR_ID,
         LAND_PC, DIST_COAST, contains(match = "2021"), geometry)

#if downloaded correctly, proceed
if(!is.null(grid)){
  grid_2021$popden <- grid_2021$TOT_P_2021/20
}

breaks <- c(
  0, 0.1, 100, 500, 1000, 2500, 5000, 10000,
  25000, max(grid_2021$popden) + 1
)

#Cut groups
grid_2021$popden_cut <- cut(grid_2021$popden, 
                            breaks = breaks, include.lowest = T)

#add labels for the interval
cut_labels <- prettyNum(breaks, big.mark = " ")[-1]
cut_labels[1] <- "0"
cut_labels[9] <- "> 25 000"

#set palette
pal <- c("black", hcl.colors(length(breaks) - 2,
                             palette = "Spectral",
                             alpha = 0.9
))

eu_map <- ggplot(data = grid_2021)+
            geom_sf(aes(fill = popden_cut), color = NA, linewidth = 0)
  
  
  
eu_map+
  coord_sf(xlim = c(2500000, 7000000),
           ylim = c(1500000, 5200000)
           )+
  scale_fill_manual(values = pal, na.value = "black",
                    name = "people per sq. km",
                    labels = cut_labels,
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = 0.5,
                      keywidth = 2,
                      title.position = "top",
                      title.hjust = 0.5,
                      label.hjust = 0.5,
                      nrow = 1,
                      byrow = T,
                      reverse = F,
                      label.position = "bottom"
                    ))+
  theme_void()+
  labs(
    title = "Population density in Europe",
    subtitle = "Grid: 20 km.",
    caption = gisco_attributions()
  )+
  theme(
    plot.background = element_rect(fill = "grey2"),
    plot.title = element_text(
      size = 18, color = "white",
      hjust = 0.5,
    ),
    plot.subtitle = element_text(
      size = 14,
      color = "white",
      hjust = 0.5,
      face = "bold"
    ),
    plot.caption = element_text(
      size = 9, color = "grey60",
      hjust = 0.5, vjust = 0,
      margin = margin(t = 5, b = 10)
    ),
    legend.text = element_text(
      size = 8,
      color = "white"
    ),
    legend.title = element_text(
      color = "white"
    ),
    legend.position = "bottom"
  )