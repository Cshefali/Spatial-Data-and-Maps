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

