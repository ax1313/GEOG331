# https://rpubs.com/boyerag/297592

# dataset used from: https://dx.doi.org/10.7265/jj4s-tq79


library(ncdf4) # package for netcdf manipulation
library(raster) # eventually replace with terra
library(terra) # package for raster / terra manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

nc_data <- nc_open('Z:/students/axie/project/G10010_V2/G10010_sibt1850_v2.0.nc')
# Save the print(nc) dump to a text file
{
  sink('project_data_v1.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude", verbose = F)
t <- ncvar_get(nc_data, "time")

seaice.array <- ncvar_get(nc_data, "seaice_conc") # store the data in a 3-dimensional array
dim(seaice.array) 

fillvalue <- ncatt_get(nc_data, "seaice_conc", "_FillValue")
fillvalue

nc_close(nc_data)

seaice.array[seaice.array == fillvalue$value] <- NA
seaice.slice <- seaice.array[, , 3] 
dim(seaice.slice)

r <- raster(t(seaice.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# r <- flip(r, direction='y')
plot(r,  main="Latitude vs. Longitude",
     xlab="longitude (degrees)", ylab="latitude (degrees)")