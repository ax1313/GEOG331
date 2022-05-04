# https://rpubs.com/boyerag/297592

# dataset used from: https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.surface.html

library(ncdf4) # package for netcdf manipulation
library(raster) # eventually replace with terra
library(terra) # package for raster / terra manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

nc_data <- nc_open('Z:/students/axie/project/air.mon.mean.nc')
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
air.slice <- air.array[, , 3] 
dim(air.slice)

r <- raster(t(air.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot(r,  main="Latitude vs. Longitude",
     xlab="longitude (degrees)", ylab="latitude (degrees)")

r_brick <- brick(air.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_brick <- flip(t(r_brick), direction='y')

toolik_lon <- 180 - 76 # 180-149.5975
toolik_lat <- 68.6275
toolik_series <- extract(r_brick, SpatialPoints(cbind(toolik_lon,toolik_lat)), method='simple')

toolik_df <- data.frame(time= nc_data[["dim"]][["time"]][["vals"]], air_temp=t(toolik_series))
ggplot(data=toolik_df, aes(x=time, y=air_temp, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("Temperature at Toolik Lake Station") +     # Set title
  theme_bw() # use the black and white theme

# Temp graph for Hamilton
hamilton_lon <- 180 - 75.5394
hamilton_lat <- 42.8165
hamilton_series <- extract(r_brick, SpatialPoints(cbind(hamilton_lon,hamilton_lat)), method='simple')

hamilton_df <- data.frame(time= nc_data[["dim"]][["time"]][["vals"]], air_temp=t(hamilton_series))
ggplot(data=hamilton_df, aes(x=time, y=air_temp, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("Temperature at Hamilton, NY") +     # Set title
  theme_bw() # use the black and white theme


air.slice.last <- air.array[, , 612]
air.slice.first <- air.array[, , 15]
air.diff <- air.slice.last - air.slice.first

r_diff <- raster(t(air.diff), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_diff <- flip(r_diff, direction='y') # is this needed?

# plot(r_diff, ylim = c(60, 90))
plot(r_diff)

v <- getValues(r_diff)

#plot(r,  main="Latitude vs. Longitude",
#     xlab="longitude (degrees)", ylab="latitude (degrees)", ylim = c(50, 90))