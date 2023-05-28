library(ggplot2)
library(sf) # Spatial data format
library(rnaturalearth) # Provides a map of countries of the entire world
library(ggspatial) # Scale bar and North arrow
library(maps) # USA
library(googleway) # Access to Google Maps APIs,
library(cowplot) # Grid plots
library(ggmap) # Google maps
library(rgdal)

setwd('.')
getwd()
theme_set(theme_bw()) # Theme appropriate for maps

fname <- system.file("gminy.shp", package="sf")

fname
nc <- st_read(fname)
