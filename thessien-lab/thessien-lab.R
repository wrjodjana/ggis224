
# set libraries
library(sf)
library(data.table)
library(raster)
library(terra)
library(tmap)
library(classInt)
library(dplyr)

# load data
polygons <- st_read("cb_2019_us_county_500k.shp")
alameda <- polygons[polygons$STATEFP=='06'& polygons$COUNTYFP=='001',]

# transform data
ply <- st_transform(alameda, crs=3310)

# California Hospital Facility Points
points <- read.csv("current-healthcare-facility-listing.csv")
head(points)

# Keep facilities with only hospital beds and General Acute Care Hospitals
points_df <- points %>%
  filter(TOTAL_NUMBER_BEDS > 1 & LICENSE_CATEGORY_DESC=="General Acute Care Hospital")

# Convert to Data Spatial Points
points_new <- st_as_sf(points_df, coords = c('LONGITUDE', 'LATITUDE'), crs = 4326)
tr_points <- st_transform(points_new, crs=3310)
ptsply <- st_intersection(tr_points, tr_alameda)

# Plot points and polygons
plot(ply$geometry)
plot(ptsply$geometry, add=T)

# Voronoi Diagram
voronoi <- 
  ptsply %>% 
  st_geometry() %>%
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract()

# Putback in original order so can easily join with source attributes
voronoi <- voronoi[unlist(st_intersects(ptsply,voronoi))]

# Plot the point voronoi polygons
plot(voronoi)

# clip the voronoi polys to the boundary

voronoi2 <- st_intersection(ply$geometry, voronoi)

# plot the clipped voronoi polygons

plot(voronoi2)
plot(ptsply$geometry, add=T, col="red", pch=20)

# Make the voronoi polygon data an sf object that has the voronoi geometry but the input attributes

voronoi_sf = st_sf(ptsply, geometry = voronoi2)    # sf object
st_set_crs(voronoi_sf, my_crs_code)
st_crs(voronoi_sf)

# Plot it to check
plot(voronoi_sf$geometry)
plot(ptsply$geometry, add=T, col="red", pch=20)
mtext("Voronoi Diagram to model proximity to hospitals in CA", side=3, line=1, adj=0, font=2)
legend("topright", legend="Points", col="red", pch=20, bty="n")






