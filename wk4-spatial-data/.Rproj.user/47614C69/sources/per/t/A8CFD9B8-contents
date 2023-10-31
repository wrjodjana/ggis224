# set directory
setwd("~/Documents/R/GGIS 224 (Environmental Data Science)/classwork_12_sept")

# installing packages
install.packages("sf")
install.packages("dplyr")
install.packages("terra")
install.packages("spData")
install.packages("tmap")

# load libraries
library(sf)
library(dplyr)
library(terra)
library(spData)
library(tmap)

plot(nz_height)

tm_shape(nz) + tm_fill(alpha = 0.5) + tm_borders() +
  tm_shape(nz_height) + tm_bubbles("elevation", size = 0.2)

canterbury <- nz |>
  filter(Name == "Canterbury")

canterbury_height <- nz_height[canterbury, op = st_touches]

tm_shape(nz) + tm_fill(alpha = 0.5) + tm_borders(alpha = 0.5) +
  tm_shape(canterbury) + tm_fill("cadetblue1") + tm_borders(alpha = 0.5) +
  tm_shape(nz_height) + tm_dots() +
  tm_shape(canterbury_height) + tm_dots("cadetblue")

mean(nz_height$elevation)
mean(canterbury_height$elevation)



  