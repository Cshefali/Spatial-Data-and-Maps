#RGraphGallery- Maps using Leaflet package
#Last Update: Jan 1, 2024
##URLS:
#https://r-graph-gallery.com/179-show-a-map-with-leaflet-r.html

library(tidyverse)
library(leaflet)
#to save map as HTML widget
library(htmlwidgets)

#working dir
working_dir <- getwd()
#path to image folder
image_dir <- paste0(working_dir, "/images/")

#initialize leaflet map with leaflet() function- gives base for the map
map1 <- leaflet::leaflet()

#add OpenStreetMap tiles (default)- a world map with OSM tiles
map1 <- leaflet::addTiles(map1)

#display map produced so far
map1

#save the widget
htmlwidgets::saveWidget(widget = map1, 
                        file = paste0(image_dir,"leaflet_OSM_worldmap.html"))

#Zoom to India
map2 <- leaflet() %>% 
          #add layer of OpenStreetMap
          addTiles() %>% 
          #zoom to India
          setView(lat = 20.5937, lng = 78.9629, zoom = 4.5)

map2

#save India map
saveWidget(widget = map2, file = paste0(image_dir,"OSM_India_map.html"))

##LINK TO ALL BACKGROUND TILES AVAILABLE WITH LEAFLET
#https://leaflet-extras.github.io/leaflet-providers/preview/index.html

#Background: NASA
map3 <- leaflet() %>% addTiles() %>% 
          setView(lat = 20.5937, lng = 78.9629, zoom = 4.5) %>% 
          addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012") %>% 
          addMarkers(lng = 87, lat = 20, popup = "hi shefali",
                     label = "hi shefali")
map3

#save nightview map
saveWidget(map3, file = paste0(image_dir,"India_map_nightoverview.html"))

#Background: Google Map
google_map_bg <- leaflet() %>% addTiles() %>% 
                  setView(lat = 20.5937, lng = 78.9629, zoom = 4) %>% 
                  addProviderTiles(provider = "Esri.WorldImagery")

google_map_bg

#Background: Gray
gray_map_bg <- leaflet() %>% addTiles() %>% 
  setView(lat = 20.5937, lng = 78.9629, zoom = 4) %>% 
  addProviderTiles(provider = "Esri.WorldGrayCanvas")

gray_map_bg

#Background- world terrain (Esri.WorldTerrain)
world_terrain_bg <- leaflet() %>% addTiles() %>% 
  setView(lat = 20.5937, lng = 78.9629, zoom = 4) %>% 
  addProviderTiles(provider = "Esri.WorldTerrain")
world_terrain_bg

#Background: World Topo Map (Esri.WorldTopoMap)
world_topo_bg <- leaflet() %>% addTiles() %>% 
  setView(lat = 20.5937, lng = 78.9629, zoom = 4) %>% 
  addProviderTiles(provider = "Esri.WorldTopoMap")

world_topo_bg

###------------
##URL- https://rstudio.github.io/leaflet/

m1 <- leaflet()