# https://rpubs.com/boyerag/297592

# dataset used from: https://dx.doi.org/10.7265/jj4s-tq79


library(ncdf4) # package for netcdf manipulation
library(raster) # eventually replace with terra
library(terra) # package for raster / terra manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

nc_data_seaice <- nc_open('Z:/students/axie/project/G10010_V2/G10010_sibt1850_v2.0.nc')
# Save the print(nc) dump to a text file
{
  sink('project_seaice_data_v1.txt')
  print(nc_data_seaice)
  sink()
}

lon <- ncvar_get(nc_data_seaice, "longitude")
lat <- ncvar_get(nc_data_seaice, "latitude", verbose = F)
t <- ncvar_get(nc_data_seaice, "time")

seaice.array <- ncvar_get(nc_data_seaice, "seaice_conc") # store the data in a 3-dimensional array
dim(seaice.array) 

fillvalue <- ncatt_get(nc_data_seaice, "seaice_conc", "_FillValue")
fillvalue

nc_close(nc_data_seaice)

seaice.array[seaice.array == fillvalue$value] <- NA
seaice.slice <- seaice.array[, , 3] 
dim(seaice.slice)

r <- raster(t(seaice.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# r <- flip(r, direction='y')
plot(r,  main="Latitude vs. Longitude",
     xlab="longitude (degrees)", ylab="latitude (degrees)")

r_brick <- brick(seaice.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

# r_brick <- flip(t(r_brick), direction='y')

toolik_lon <- 360-149.5975
toolik_lat <- 68.6275
toolik_series <- extract(r_brick, SpatialPoints(cbind(toolik_lon,toolik_lat)), method='simple')

toolik_df <- data.frame(time= nc_data_seaice[["dim"]][["time"]][["vals"]], air_temp=t(toolik_series))
ggplot(data=toolik_df, aes(x=time, y=air_temp, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("Temperature at Toolik Lake Station") +     # Set title
  theme_bw() # use the black and white theme

