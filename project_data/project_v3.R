# https://www.tutorialspoint.com/r/r_binary_files.htm

# These libraries were copied from previous scripts - May not be needed
library(ncdf4) # package for netcdf manipulation
library(raster) # eventually replace with terra
library(terra) # package for raster / terra manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

read.filename <- file("Z:/students/axie/project/bt_19790820_n07_v3.1_n.bin", "rb")
column.names <- readBin(read.filename, character(), n = 3)
bindata <- readBin(read.filename, integer())