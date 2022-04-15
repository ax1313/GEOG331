# https://rpubs.com/boyerag/297592

library(ncdf4) # package for netcdf manipulation
library(raster) # eventually replace with terra
library(terra) # package for raster / terra manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

nc_data <- nc_open('Z:/students/axie/project/air.2x2.1200.mon.anom.comb.nc')
# Save the print(nc) dump to a text file
{
  sink('project_data_v1.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

air.array <- ncvar_get(nc_data, "air") # store the data in a 3-dimensional array
dim(air.array) 

fillvalue <- ncatt_get(nc_data, "air", "_FillValue")
fillvalue

nc_close(nc_data)

air.array[air.array == fillvalue$value] <- NA
air.slice <- air.array[, , 400] 
dim(air.slice)

r <- raster(t(air.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r,  main="Latitude vs. Longitude",
     xlab="longitude (degrees)", ylab="latitude (degrees)", xlim = c(-180, 180))

