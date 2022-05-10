# Code based from: https://rpubs.com/boyerag/297592
# https://ropensci.org/blog/2019/11/05/tidync/


library(ncdf4) 
library(raster) # Use for temperature data
library(tidync) # Use for sea-ice concentration data
library(rgdal)
library(ggplot2)
library(ncmeta)

# Temperature (monthly mean) data (air.mon.mean.nc)
# Data starts from 

# load in file and llongitude, latitude, and time values
nc_mean_temp <- nc_open('Z:/students/axie/project/air.mon.mean.nc')
lon <- ncvar_get(nc_mean_temp, "lon")
lat <- ncvar_get(nc_mean_temp, "lat", verbose = F)
t <- ncvar_get(nc_mean_temp, "time")

air.array <- ncvar_get(nc_mean_temp, "air") # store the data in a 3-dimensional array
fillvalue <- ncatt_get(nc_mean_temp, "air", "_FillValue")

nc_close(nc_mean_temp)

# Loop during January
jan_val <- 13 # 13 for January, 16 for April, 19 for July, 22 for October
air.jan.first <- air.array[, , 1] # 1 for January, 4 for April, 7 for July, 10 for October
lat_index <- 6
lon_index <- round((180 - 100.1140) / 2.5) + 1
temp_jan = rep()
while (jan_val <= nc_mean_temp[["dim"]][["time"]][["len"]]) {
  air.jan.last <- air.array[, , jan_val] # place loop value here
  air.diff <- air.jan.last - air.jan.first
  temp_jan <- append(temp_jan, air.diff[lon_index, lat_index])
  jan_val = jan_val + 12
}
years = 1948:2021
plot(years, temp_jan, xlab = "year", ylab = "temperature (degC)", main = "January Temperatures in Arctic Center")


# Temperature (daily) data



# Sea ice data (Use tidync)
nc_seaice <- tidync("Z:/students/axie/project/G10010_V2/G10010_sibt1850_v2.0.nc")

nc_grids(nc_seaice)
nc_vars("Z:/students/axie/project/G10010_V2/G10010_sibt1850_v2.0.nc")
