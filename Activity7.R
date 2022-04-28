#install.packages(c("caret","randomForest"))
library(terra)
library(caret)
library(randomForest)

#set up working directory for oneida data folder on the server
setwd("Z:/data/oneida/")

#read in Sentinel data

# list files from sentinel satellite image files
f <- list.files(path = "sentinel/", pattern = "T18", full.names = T)

# read the list of files as a single multi-band spatRaster
rsdat <- rast(f)

# create a vector of band names so we can keep track of them
b <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")

#set band names in our raster
names(rsdat) <- b

# read the cloud mask data file
clouds <- rast("sentinel/MSK_CLDPRB_20m.tif")

#read in validation data
algae <- vect("Oneida/algae.shp")
agri <- vect("Oneida/agriculture.shp")
built <- vect("Oneida/built.shp")
forest <- vect("Oneida/forest.shp")
water <- vect("Oneida/water.shp")
wetlands <- vect("Oneida/wetlands.shp")