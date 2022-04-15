# https://rpubs.com/boyerag/297592

library(ncdf4) # package for netcdf manipulation
library(terra) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

nc_data <- nc_open('Z:/students/axie/project/air.2x2.250.mon.anom.comb.nc')
# Save the print(nc) dump to a text file
{
  sink('project_data_v1.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

ndvi.array <- ncvar_get(nc_data, "air") # store the data in a 3-dimensional array
dim(ndvi.array) 

