# set working directory
setwd("~/Documents/R/GGIS 224 (Environmental Data Science)/wk5-park-exercise")

# install packages
library(sf)
library(tmap)
library(terra)
library(dplyr)
library(spData)

# load dataset
tracts <- st_read("~/Documents/R/GGIS 224 (Environmental Data Science)/wk5-park-exercise/dataset/chives-data copy.geojson")
parks <- st_read("~/Documents/R/GGIS 224 (Environmental Data Science)/wk5-park-exercise/dataset/Parks.geojson")

parks
tracts

# spatial join
parks_in_tracts <- st_join(parks, tracts)
parks_in_tracts

dim(parks_in_tracts)
# Count Number of Parks by Tract
no_of_parks <- parks_in_tracts %>%
  group_by(geoid) %>%
  summarize(num_parks = n(), replace = TRUE)

st_join(parks_in_tracts, no_of_parks)

# Develop a 1-mile park buffer in Chicago
parks_buffered <- parks_in_tracts %>%
  st_buffer(1609.34)

parks_buffered

# Count number of park buffers by tract in Chicago
parkbuffer_in_tracts <- st_join(parks_buffered, tracts)
parkbuffer_in_tracts

no_of_park_buffers <- parkbuffer_in_tracts %>%
  group_by(geoid.x) %>%
  summarize(num_parkbuffers = n())

no_of_park_buffers

# Calculate proportion of park covering a census tract.





