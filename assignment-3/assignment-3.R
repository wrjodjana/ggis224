# Set Directory
setwd("~/Documents/R/ggis-224/crime-data-lab")

# load libraries
library(sf)
library(tmap)
library(leaflet)
library(data.table)
library(tidyverse)

# load data
LAcrime<-fread("LAPD2015_Violent.csv", header = T)
head(LAcrime)

# identify unique code
unique(LAcrime$`Crime Code Description`)

# subset data
LAcrime.df<- as.data.frame(LAcrime)

# base R subset
s1<-subset(LAcrime.df,LAcrime.df$`Crime Code Description`== "CRIMINAL HOMICIDE")

# binding rows
LAcrime.hom<-rbind(s1)

# inspecting data
head(s1)

# save cleaned data
write.csv(LAcrime.hom,"LAcrime_hom.csv")
