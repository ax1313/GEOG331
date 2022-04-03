# https://rpubs.com/boyerag/297592

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

nc_data <- nc_open('project_data/air.2x2.250.mon.anom.comb.nc')
# Save the print(nc) dump to a text file
{
  sink('project_data_v1.txt')
  print(nc_data)
  sink()
}
