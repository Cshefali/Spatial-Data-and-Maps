#Map of Europes built with giscoR package
#Author- Shefali C.
#Last Updated- Aug 25, 2023

library(tidyverse)
library(sf)
library(giscoR)

#set the cache directory.

#path to data directory
data_dir_path <- paste0(getwd(), "/data/gisco_datasets")
gisco_set_cache_dir(data_dir_path)
