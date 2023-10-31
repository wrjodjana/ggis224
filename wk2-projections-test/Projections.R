###################
# Environment Setup
###################

# set working directory
setwd("~/Documents/R/GGIS 224 (Environmental Data Science)/projections-test")

# install packages
install.packages("sf")
install.packages("tmap")
if (!require('devtools')) install.packages('devtools')
devtools::install_github('rstudio/leaflet')

# load the libraries
library(sf)
library(tmap)

# load spatial data
Chi_tracts <- st_read("geo_export_4d7d8529-c4a9-4577-b5b5-5ce9bf03a84c.shp")

# non-spatial & spatial views
head(Chi_tracts)
plot(Chi_tracts)

# spatial data structure
str(Chi_tracts)
st_crs(Chi_tracts)

# Mollweide Coordinate Reference Systems
# perserves area across the globe
  Chi_tracts.moll <- st_transform(Chi_tracts, crs="ESRI:54009")
  plot(st_geometry(Chi_tracts.moll), border = "gray", lwd = 2, main = "Mollweide", sub="preserves areas")

# Winkel Coordinate Reference Systems
# minimal distortion for area, distance and angles
  Chi_tracts.54019 <- st_transform(Chi_tracts, crs="ESRI:54019")
  plot(st_geometry(Chi_tracts.54019), border = "gray", lwd = 2, main = "Winkel", sub="minimal distortion")
  
# Old Hawaiian UTM Zone 4N
  Chi_tracts.Hawaii = st_transform(Chi_tracts, crs="ESRI:102114")
  plot(st_geometry(Chi_tracts.Hawaii), border = "gray", lwd = 2, main = "Old Hawaiian UTM Zone 4N", sub="wrong projection!")

# EPSG: 3435
# uses distance as feet/metres
  Chi_tracts.3435 <- st_transform(Chi_tracts, "EPSG:3435")
  st_crs(Chi_tracts.3435)
  plot(st_geometry(Chi_tracts.3435), border = "gray", lwd = 2, main = "NAD83 / Illinois East (ftUS)", sub="topo mapping & survey use")

# TMAP  


# Tracts with semi-transparent borders
tm_shape(Chi_tracts) + tm_borders(alpha=0.5) 

# Fill tracts with light gray
tm_shape(Chi_tracts) + tm_fill(col = "gray90") + tm_borders(alpha=0.2, col = "gray10") +
  tm_scale_bar(position = ("left"), lwd = 0.8) +
  tm_layout(frame = F)

# Arrange multiple maps
tracts.4326 <- tm_shape(Chi_tracts) + tm_fill(col = "gray90") +
  tm_layout(frame = F, title = "EPSG 4326")
tracts.54019 <- tm_shape(Chi_tracts.54019) + tm_fill(col = "gray90") +  tm_layout(frame = F, title = "EPSG 54019")
tmap_arrange(tracts.4326, tracts.54019)

# Interactive mode
tmap_mode("view")

# Plot mode
tmap_mode("plot")

# Same map previously
tm_shape(Chi_tracts) + tm_fill(col = "gray90") + tm_borders(alpha=0.2, col = "gray10") +
  tm_scale_bar(position = ("left"), lwd = 0.8) +
  tm_layout(frame = F)

# Make tracts more transparent
tm_shape(Chi_tracts) + tm_fill(col = "gray90", alpha = 0.5) + tm_borders(alpha=0.2, col = "gray10") + tm_scale_bar(position = ("left"), lwd = 0.8) +  tm_layout(frame = F)


# Overlay Zip Boundaries
Chi_Zips = st_read("geo_export_694099f7-f504-4744-b8f9-f9041ed608ba.shp")

# Layer the zip codes
## FIRST LAYER: CENSUS TRACT BOUNADRIES
tm_shape(Chi_tracts.3435) + tm_fill(col = "gray90") +
  tm_borders(alpha=0.2, col = "gray10") + 
  
## SECOND LAYER: ZIP CODE BOUNDARIES WITH LABEL
tm_shape(Chi_Zips) + tm_borders(lwd = 2, col = "#0099CC") +
  tm_text("zip", size = 0.7) +
  
  ## MORE CARTOGRAPHIC STYLE
  tm_scale_bar(position = ("left"), lwd = 0.8) +
  tm_layout(frame = F)






  
  