# load the terra package
library(terra)

#set working directory
setwd("Z:/students/axie/GEOG331")

# read a raster from file
p <- rast("Z:/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")

# plot the raster
plot(p)

# plot an RGB rendering of the data
plotRGB(p, r = 3, g = 2, b = 1,
        scale = 65535,
        stretch = "hist")

# read file with field observations of canopy cover
tree <- read.csv("Z:/data/rs_data/siberia_stand_data.csv",
                 header = T)

# convert to vector object using terra package
gtree <- vect(tree, geom = c("Long", "Lat"), "epsg:4326")

# project the data to match the coordinate system of the raster layer
gtree2 <- project(gtree,p)

# run this AFTER plotting RGB graph and declaring gtree2
plot(gtree2, add = T, col = "red")

# create a polygon from the extent of the points
b <- as.lines(ext(gtree), "epsg:4326")

# reproject the polygons to the same projection as our raster
b2 <- project(b,crs(p))

# run this AFTER declaring b2
plot(b2, add = T)

# buffer the extent by 200m
b3 <- buffer(b2, width = 200)


