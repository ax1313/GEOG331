# GEOG331 Activity 2 Script
# 01/31/22

# Make a vector of tree heights in meters
heights <- c(30, 41, 20, 22)

#convert to cm
heights_cm = heights * 100
heights_cm

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat