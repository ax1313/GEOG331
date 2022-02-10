# GEOG331 Activity 2 Script
# 01/31/22

# Make a vector of tree heights in meters
heights <- c(30, 41, 20, 22)

#get more info on the matrix function
#help(matrix)

#convert to cm
heights_cm = heights * 100
heights_cm

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#subset the matrix to look at row 1, column2
Mat.bycol[1,2]

#look at all values in row 1
Mat.bycol[1,]

#look at all values in column 2
Mat.bycol[,2]

datW <- read.csv("data/noaa_weather/2011124.csv", stringsAsFactors = T)
str(datW)