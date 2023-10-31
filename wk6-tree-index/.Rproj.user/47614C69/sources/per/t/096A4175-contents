
setwd("~/Documents/R/GGIS 224 (Environmental Data Science)/Tree Index Building")

###########################
## Index Building
###########################

library(sf)

trees.all <- st_read("tree_sst_master_tracts_v2_0_1.shp")

head(trees.all)

variables <- c("geoid", "svi_pecent", "trees_crow", "logtraf",
               "urban_floo", "heatisl","nn_q3_pm2_", "asthma_5yr")

trees <- trees.all[,variables]
summary(trees)

head(trees)

#centers and/or scales the columns of a numeric matrix
#correct for directionality
trees$svi <- -scale(trees$svi_pecent)
trees$tre <- scale(trees$trees_crow)
trees$car <- -scale(trees$logtraf)
trees$fld <- -scale(trees$urban_floo)
trees$sft <- -scale(trees$heatisl)
trees$pm2 <- -scale(trees$nn_q3_pm2_)
trees$ast <- -scale(trees$asthma_5yr)

head(trees)

library(tmap) 

tm_shape(trees) + tm_fill(col = "trees_crow", style = "jenks")
tm_shape(trees) + tm_fill(col = "nn_q3_pm2_", style = "jenks")
tm_shape(trees) + tm_fill(col = "asthma_5yr", style = "jenks")

#Equally weighted index 
trees$index <-  trees$svi + trees$tre + trees$fld + 
                trees$sft + trees$pm2 + trees$car

trees$index <- as.numeric(trees$index)

###########################
## Visual Analysis
###########################


tm_shape(trees) + tm_fill(col = "index")

tm_shape(trees) + tm_fill(col = "index", midpoint = NA)

tm_shape(trees) + tm_fill(col = "index", style = "jenks",
                          midpoint = NA)

tm_shape(trees) + tm_fill(col = "index", style = "jenks",
                          midpoint = NA,
                          title = "tree index")


#Weight index according to conceptual model
trees$inde2x <- 0.5*trees$svi + 0.2*trees$tre + 
  0.3*((trees$fld + trees$sft + trees$pm2 + trees$car)/4)

trees$inde2x <- as.numeric(trees$inde2x)

tm_shape(trees) + tm_fill(col = "inde2x", style = "jenks",
                          midpoint = NA,
                          title = "tree index")

###########################
## Correlation Analysis
###########################

library(corrplot)

variables2 <- c("svi_pecent", "trees_crow", "logtraf",
               "urban_floo", "heatisl","nn_q3_pm2_", "asthma_5yr")

trees2 <- trees[,variables2]
trees2 <- na.omit(st_drop_geometry(trees2))
str(trees2)

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
tree.C <- cor(trees2, method="spearman")

corrplot(tree.C, method = "circle")
corrplot.mixed(tree.C,  lower.col = "black", number.cex = .7)

names(trees2) <-c("svi", "trees", "traffic",
                  "flood", "heat","pm2", "asthma")

###########################
## Scaled Index from 0 to 100
###########################

library(scales)

trees$svi2 <- rescale(trees$svi_pecent, to = c(0,100)) #higher = worse
trees$tre2 <- rescale(trees$trees_crow, to = c(100,0)) #lower = worse
trees$car2 <- rescale(trees$logtraf, to = c(0,100)) #higher = worse
trees$fld2 <- rescale(trees$urban_floo, to = c(0,100)) #higher = worse
trees$sft2 <- rescale(trees$heatisl, to = c(0,100)) #higher = worse
trees$pm22 <- rescale(trees$nn_q3_pm2_, to = c(0,100)) #higher = worse
trees$ast2 <- rescale(trees$asthma_5yr, to = c(0,100)) #higher = worse

tm_shape(trees) + tm_fill(col = "tre2", style = "jenks")
tm_shape(trees) + tm_fill(col = "pm22", style = "jenks")
tm_shape(trees) + tm_fill(col = "svi2", style = "jenks")

#Equally weighted index 
trees$index2 <-  (trees$svi2 + trees$tre2 + trees$fld2 + 
  trees$sft2 + trees$pm22 + trees$car2)/6

trees$index2 <- as.numeric(trees$index2)

tm_shape(trees) + tm_fill(col = "index2", style = "jenks", n=6, pal = "BuPu")

#Weight index according to conceptual model
trees$physvuln = (trees$fld2 + trees$sft2 + trees$pm22 + trees$car2)/4
trees$index22 <- 0.6*trees$tre2 + 0.2*trees$physvuln + 0.2*trees$svi2

trees$index22 <- as.numeric(trees$index22)

tm_shape(trees) + tm_fill(col = "index22", style = "jenks",
                          pal = "BuPu",
                          title = "tree index")
