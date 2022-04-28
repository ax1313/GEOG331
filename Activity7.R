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

# reclassify the cloud mask so that pixel values below 60% become 1
# and values over 60 become NA
cloudsF <- classify(clouds, matrix(c(-Inf,60,1,60,Inf,NA), ncol = 3, byrow = T))

# use the cloud mask to remove NA pixels from the reflectance data
rsmask <- mask(rsdat,cloudsF)

#set seed so samples always the same
set.seed(12153)
#randomly select the data in each dataset to be  used
sampleType <- rep("train",120)
#samples to randomly convert to validation data
sampleSamp <- sample(seq(1,120),60)
#convert these random samples from training to validation
sampleType[sampleSamp] <- "valid"

#set up table with coordinates and data type (validate or train) for each point
landExtract <-  data.frame(landcID = rep(seq(1,6),each=120),
                           x=c(crds(algae)[,1],crds(water)[,1],crds(agri)[,1],crds(built)[,1],crds(forest)[,1],crds(wetlands)[,1]),
                           y=c(crds(algae)[,2],crds(water)[,2],crds(agri)[,2],crds(built)[,2],crds(forest)[,2],crds(wetlands)[,2]))
#add sample type
landExtract$sampleType <- rep(sampleType, times=6)

#create id table that gives each landcover an ID
landclass <- data.frame(landcID= seq(1,6),
                        landcover = c("algal bloom", "open water","agriculture","built","forest","wetlands"))

#extract reflectance values for our sample points
rasterEx <- data.frame(extract(rsmask,landExtract[,2:3]))[,-1]

#combine point information with raster information
dataAll <- cbind(landExtract,rasterEx)
#preview
head(dataAll)

#remove missing data
dataAlln <- na.omit(dataAll)

#subset into two different data frames
trainD <- dataAlln[dataAlln$sampleType == "train",]
validD <- dataAlln[dataAlln$sampleType == "valid",]

#Kfold cross validation
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:sqrt(9)) # number of variables available for splitting at each tree node
