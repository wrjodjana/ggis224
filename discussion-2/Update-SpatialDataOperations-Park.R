    library(sf)
library(tidyverse)
library(tmap)

setwd("~/Documents/R/GGIS 224 (Environmental Data Science)/discussion-2")

chiparks <- st_read("Parks.geojson")
chiparks

#Map parks as points
tm_shape(chiparks) + tm_dots()

#Read in Chicago tract level data from Assignment 1
chitracts <- st_read("~/Documents/R/GGIS 224 (Environmental Data Science)/Assignment-1/chives-data.geojson")

#Map tracts and parks to confirm they overlap
tm_shape(chitracts) + tm_fill() + 
  tm_shape(chiparks) + tm_dots(alpha = 0.4) 

#Prepare data for a spatial data operation => Check CRS
st_crs(chiparks)
st_crs(chitracts)

#Transform to CRS of chiparks
#chitracts <- st_transform(chitracts, st_crs(chiparks))

head(chiparks)
head(chitracts)

#Spatially join parks with tracts; data remains as points/parks.
#What would be another way of doing this?
pipr <- st_join(chiparks, chitracts)
head(pipr)

#Identify which column will be our Tract ID within points
head(chitracts) #geoid

#Aggregate count of parks by GEOID. What are othe ways to do this?
ptcount <- as.data.frame(table(pipr$geoid))
head(ptcount)

#Rename column fields to make for easy merging back to Tracts file
names(ptcount) <- c("geoid", "ParkCt")
head(ptcount)

#Merge count variable back to tracts
areas<- merge(chitracts, ptcount, by="geoid", all = TRUE)
head(areas)

tm_shape(areas) + tm_fill("ParkCt", style = "jenks", n=6, alpha = 0.8)


#Load data wrangling library
library(dplyr)

#Merge data to community areas as new spatial variable
ParkCom <- areas %>% 
  group_by(community) %>%
  summarize(Parks = sum(ParkCt, na.rm = TRUE),
            TotTracts = n())

#Map new spatial variable (ie. Parks)
tm_shape(ParkCom) + tm_fill("Parks", style = "jenks",
                            pal = "BuPu", n=6, alpha = 0.8)


###############
#Bonus Challenge
###############

# 1. Find Chicago parks from the Chicago Data Portal. Download as shp. 
# 1. Import shapefile of parks, and do a spatial join of these parks & tracts.
# 1. How does the spatial variable (Total #Parks in Tracts) differ?

###############
#Bonus Visualization 
###############

#Another way of visualizing -- use ggmap & ggplot

#install.packages("ggmap")
#install.packages("ggplot2")
library(ggmap)
library(ggplot2)

# retrieving roadmap of Chicago
Chi_map <- get_map(getbb("Chicago"), maptype = "roadmap")

ggmap(Chi_map) +
  geom_sf(
    data = park$osm_points,
    inherit.aes = FALSE,
    colour = "#08519c",
    alpha = .5,
    size = 1
  ) +
  labs(
    title = "Parks in Chicago",
    x = "Latitude",
    y = "Longitude"
  )
