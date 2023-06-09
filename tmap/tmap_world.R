##URL- https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html
##AUTHOR- Shefali C.
##DATE- April 19, 2023

library(dplyr)
library(tmap)
library(sf)
#for color palettes
library(viridis)

#following function sets the options globally. It applies to all maps
#in the current session.
#tmap_options(bg.color = "skyblue", legend.text.color = "black")


data("World")
#setting map mode to interactive: mode = "view"
#tmap::tmap_mode(mode = "view")

###-------------------------------
#CREATING A NEW BOUNDING BOX TO ADJUST TITLE & LEGEND IN THE MAP.
#THE DEFAULT BBOX ADDS TITLE & LEGEND ON TOP OF MAP ITSELF RENDERING A MESSY PLOT
#FOLLOWING CODE ADDS A LITTLE MORE SPACE TO THE ORIGINAL BBOX DIMENSIONS
#Help- https://stackoverflow.com/questions/60892033/how-do-you-position-the-title-and-legend-in-tmap

#st_bbox() returns (x,y) dimensions of 4 corners of current bounding box
bbox_new <- sf::st_bbox(World)

#range of x axis values (gives length of bounding box rectangle)
xrange <- bbox_new$xmax - bbox_new$xmin
#range of y values (width of the rectangle)
yrange <- bbox_new$ymax - bbox_new$ymin

#adding 25% more space to the right and top of the bbox.
#i want to place the legend to the right of the plot, so need space in RHS.
#space will be added to top of the plot for title.

#bbox_new[3] means xmax; xmax is towards RHS of plot.
bbox_new[3] <- bbox_new[3] + (0.3*xrange)
#bbox_new[4] means ymax; ymax is the upward y-axis on the plot
bbox_new[4] <- bbox_new[4] + (0.5*yrange)

##converting the bounding box into sf polygon
#the st_bbox() returns a vector with dimensions. bbox argument of tm_shape()
#requires a sf object. Hence the conversion.
bbox_new <- bbox_new %>% 
              sf::st_as_sfc()

########--------

#world map with HPI values
#setting mode of map to static
tmap_mode(mode = "plot")
tmap::tm_shape(World, bbox = bbox_new)+
  tm_polygons(col = "HPI", palette = "YlOrBr")+
  tm_layout(
            #inner.margins = c(0, 0.02, 0.02, 0.02),
            title = "Countries by happiness index",
            title.size = 1,
            title.position = c("center","top"),
            legend.position = c("right","center"))+
  tm_credits(text = "Source: https://www.naturalearthdata.com/",
             position = c("LEFT","BOTTOM"),
             fontface = "bold")

#style is a specific theme type in tmap. many options available
tmap_style("classic")

#world map with life expectancy
tmap::tm_shape(World)+
  tm_polygons(col = "life_exp")

tmap_style("natural")
#world map with well-being index
tmap::tm_shape(World)+
  tm_polygons(col = "well_being")

tmap_style("cobalt")
#world map with inequality
tmap::tm_shape(World)+
  tm_polygons(col = "inequality")


#world map with economy type
tmap::tm_shape(World)+
  tm_polygons(col = "economy")+
  #since map is interactive, tm_view() has to be used to set legend format
  tm_view(
    view.legend.position = c("right","bottom"),
  )

#Adding multiple layers to the plot
data("World", rivers, metro, land)

#tmap mode set to static
tmap_mode(mode = "plot")

#Layer 1 creates thematic map with land elevation
#Layer 2 adds white borders around country polygon with country abbreviation
#Layer 3 adds points for metro cities in each country
#tm_legend removes the legend from bottom left corner of map

#tmap_style("albatross")
tm_shape(land)+
  tm_raster("elevation", palette = terrain.colors(10))+
tm_shape(World)+
  tm_borders(col = "white", lwd = 1)+
  tm_text(text = "iso_a3", size = "area")+
tm_shape(metro)+
  tm_symbols(col = "red", size = "pop2020", scale = 0.5)+
tm_legend(show = FALSE)

##The Warning message: update crs can be avoided by explicitly stating the 
#crs value. The df geometry column contains CRS projection in
#old format- proj4string, This format has been deprecated. 
#Following code sets the CRS to WKT format.

#check the format of CRS in spatial object
str(attr(metro$geometry, "crs"))

#change the format of projection.

#making a copy of the tmap dataframe
metro <- metro
#from the attribute, we can see the CRS is ESPG, code is 4326. 
#explicitly setting crs to 4326
sf::st_crs(metro) <- 4326

#now checking the CRS format again
str(attr(metro$geometry, "crs"))

#Re-creating the above layered map; this time no warning appears
tm_shape(land)+
  tm_raster("elevation", palette = terrain.colors(10))+
tm_shape(World)+
  tm_borders(col = "white", lwd = 1)+
  tm_text(text = "iso_a3", size = "area")+
tm_shape(metro)+
  tm_symbols(col = "red", size = "pop2020", scale = 0.5)+
tm_legend(show = FALSE)

##CREATING FACETS- 3 WAYS

#Method 1- assign multiple variables in tm_polygons() col argument.

#Interactive mode- on
tmap_mode(mode = "view")
tm_shape(World)+
  tm_polygons(col = c("HPI","economy"))+
  tm_facets(sync = TRUE, ncol = 2)+
  tm_view(
    view.legend.position = c("right","bottom")
  )

#Method 2-assign a variable in df to "by" argument of tm_facets(). 
tmap_mode(mode = "plot")
tm_shape(World)+
  tm_polygons(col = "economy", palette = "RdYlBu")+
  tm_facets(by = "continent")

#Method 3- Using tmap_arrange() function

#using Netherlands municipality dataset for this one
data("NLD_muni")

##CRS format of NLD_muni is deprecated version proj4string. Explicitly changing
#it to wkt format, to avoid warning.

nld_muni <- NLD_muni
#In "help", it is mentioned that projection used for Netherlands dataframes is 
#Rijksdriehoekstelsel.
#EPSG CRS code for Rijksdriehoekstelsel projection is 28992.
sf::st_crs(nld_muni) <- 28992 

#creating facets using tmap_arrange()
tm1 <- tm_shape(nld_muni)+
        tm_polygons(col = "population", convert2density = T)

tm2 <- tm_shape(nld_muni)+
        tm_bubbles(size = "population", col = "midnightblue")

tmap_arrange(tm1, tm2)

##Using tm_raster

#custom palette
pal8 <- c("#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", "#999999", 
          "#E31A1C", "#E6E6E6", "#A6CEE3")

#world map with land cover and metro cities
tm_shape(land, ylim = c(-88,88)) +
  tm_raster("cover_cls", palette = pal8, title = "Global Land Cover")+
tm_shape(metro) +
  tm_dots(col = "#E31A1C")+
tm_shape(World)+
  tm_borders(col = "black")+
  #scale argument reduces the size of legend box
tm_layout(scale = .5,
          legend.position = c("left","bottom"),
          legend.bg.color = "white",
          legend.bg.alpha = .2,
          legend.frame = "gray50"
          )

#below palette contains 20 colors to represent 20 cover-types in land df
#another custom palette
pal20 <- c("#003200", "#3C9600", "#006E00", "#556E19", "#00C800", "#8CBE8C",
           "#467864", "#B4E664", "#9BC832", "#EBFF64", "#F06432", "#9132E6",
           "#E664E6", "#9B82E6", "#B4FEF0", "#646464", "#C8C8C8", "#FF0000",
           "#FFFFFF", "#5ADCDC")

#land cover represents a finer layer of cover type, 20 different categories
tm_shape(land)+
  tm_raster("cover", palette = pal20, title = "Global Land Cover")+
  tm_layout(scale = .4,
            legend.position = c("left", "bottom"))

#creating map for land elevation

land_cover <- tm_shape(land)+
                tm_raster("elevation", 
                          palette = viridis(n = 15, option = "mako", direction = -1))+
                tm_layout(scale = .5,
                          legend.position = c("left", "bottom"))

#creating map for land elevation in interactive mode
tmap_mode(mode = "view")

tm_shape(land)+
  tm_raster("elevation",
            palette = viridis(n = 15, option = "mako", direction = -1))+
  tm_view(view.legend.position = c("left", "bottom"))

#Creating tiled basemaps
tmap_mode(mode = "view")

tm_basemap(server = "Stamen.Watercolor")+
  tm_shape(metro)+
  #scale arguments makes sure bubble size doesn't get too large.
  tm_bubbles(col = "red", size = "pop1950", scale = 0.5)+
  #adds a layer with names of all places.
  tm_tiles("Stamen.TonerLabels")

#to see what all options have been changed from default values.
tmap_options_diff()
#resetting options to default value
tmap_options_reset()

#Exporting and saving maps- static and interactive versions
tmap_save(tm = land_cover, filename = "images/world_land_cover.png")

#saving as stand-alone html file
tmap_save(tm = land_cover, filename = "images/world_land_cover.html")

#creating thematic map with one function call
qtm(World, fill = "well_being", 
    fill.pallete = "plasma")

#Checking what this function does!
#tmap_tip()