###############
# SPATIAL DATA OPERATIONS PRACTICE:
# Parks in Chicago with Crowdsourced Data
###############
# compiled by Marynia Kolak for Environmental Data Science @UIUC, Fall 2022
###############
# with highlight code chunks from: https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
# and a great park tutorial from: https://rspatialdata.github.io/osm.html
# and info on OSM data on parks from https://wiki.openstreetmap.org/wiki/Tag:leisure%3Dpark
###############

install.packages("osmdata")
library(osmdata)

#View features available
available_features()

#Select options within the 'leisure' category
leisure <- available_tags("leisure")
leisure

#Create an overpass query to grab data for City
q <- getbb("Chicago") %>%  #Specify City
  opq() %>% #Builds an overpass query
  add_osm_feature("leisure", "park") #Specify Tags

#Query Structure
str(q) 

#Returns overpass query as sf object 
park <- osmdata_sf(q)
park

#Another way of doing the OSMDATA query:
#chi_parks <- getbb("Chicago") %>%
#  opq() %>%
#  add_osm_feature(key = "leisure", value = "park") %>%
#  osmdata_sf()

#Load libraries for mapping & spatial data wrangling
library(tmap)
library(sf)

#Extract just point data of parks, and rename
chiparks <- (park$osm_points)

#Confirm structure is an sf object
str(chiparks)

#Map parks as points
tm_shape(chiparks) + tm_dots()

#Read in Chicago tract level data from Assignment 1
chitracts <- read_sf("chives-data.geojson")

#Map tracts and parks to confirm they overlap
tm_shape(chitracts) + tm_fill() + 
  tm_shape(chiparks) + tm_dots(alpha = 0.4) 

#Prepare data for a spatial data operation => Check CRS
st_crs(chiparks)
st_crs(chitracts)

#Transform to CRS of chiparks
chitracts <- st_transform(chitracts, st_crs(chiparks))

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
tm_shape(ParkCom) + tm_fill("Parks", style = "jenks", n=6, alpha = 0.8)


###############
#Bonus Challenge
###############

# 1. Find Chicago parks from the Chicago Data Portal. Download as shp. 

parks <- st_read("geo_export_57b2c3bb-96b4-4b61-a693-b10a1ebb5e87.shp")
parks

# 1. Import shapefile of parks, and do a spatial join of these parks & tracts.
st_join(chitracts, parks, c("gisobjid", "geoid"))

# 1. How does the spatial variable (Total #Parks in Tracts) differ?





###############
#Bonus Visualization 
###############

#Another way of visualizing -- use ggmap & ggplot

install.packages("ggmap")
install.packages("ggplot2")
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
