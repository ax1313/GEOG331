# Code adapted from: https://rpubs.com/boyerag/297592

library(ncdf4) 
library(raster) # Use for temperature data
library(rgdal)
library(ggplot2)

# Temperature (monthly mean) data (air.mon.mean.nc)
# Data starts from January 1, 1948 and ends on March 1, 2022

# load in file and llongitude, latitude, and time values
nc_mean_temp <- nc_open('Z:/students/axie/project/air.mon.mean.nc')
lon <- ncvar_get(nc_mean_temp, "lon")
lat <- ncvar_get(nc_mean_temp, "lat", verbose = F)
t <- ncvar_get(nc_mean_temp, "time")

air.array <- ncvar_get(nc_mean_temp, "air") # store the data in a 3-dimensional array
fillvalue <- ncatt_get(nc_mean_temp, "air", "_FillValue")

nc_close(nc_mean_temp)

time_length = nc_mean_temp[["dim"]][["time"]][["len"]]

get_month_vals <- function(first_month_val, lat_index, lon_index, array, length) {
  new_lon_index = round((180 - lon_index) / 2.5) + 1
  second_month_val <- first_month_val + 12
  air.first <- array[, , first_month_val]
  temp <- rep()
  while (second_month_val <= length) {
    air.last <- air.array[, , second_month_val] # place loop value here
    air.diff <- air.last - air.first
    temp <- append(temp, air.diff[new_lon_index, lat_index])
    second_month_val <- second_month_val + 12
  }
  return(temp)
}

plot_vals <- function(month, month_vals, title) {
  years = 1949:2021
  if (month <= 3) {
    years = 1949:2022
  }
  plot(years, month_vals, xlab = "year", ylab = "temp difference (degC)", main = title)
}
  
# Adjust these values for different latitude / longitude 
lat_adjust = 6
lon_adjust = 100.1140

# January
temp_jan <- get_month_vals(1, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(1, temp_jan, "January Difference in Temperatures from 1948")

# February
temp_feb <- get_month_vals(2, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(2, temp_feb, "February Difference in Temperatures from 1948")

# March
temp_mar <- get_month_vals(3, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(3, temp_mar, "March Difference in Temperatures from 1948")

# April
temp_apr <- get_month_vals(4, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(4, temp_apr, "April Difference in Temperatures from 1948")

# May
temp_may <- get_month_vals(5, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(5, temp_may, "May Difference in Temperatures from 1948")

# June
temp_jun <- get_month_vals(6, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(6, temp_jun, "June Difference in Temperatures from 1948")

# July
temp_jul <- get_month_vals(7, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(7, temp_jul, "July Difference in Temperatures from 1948")

# August
temp_aug <- get_month_vals(8, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(8, temp_aug, "August Difference in Temperatures from 1948")

# September
temp_sep <- get_month_vals(9, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(9, temp_sep, "September Difference in Temperatures from 1948")

# October
temp_oct <- get_month_vals(10, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(10, temp_oct, "October Difference in Temperatures from 1948")

# November
temp_nov <- get_month_vals(11, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(11, temp_nov, "November Difference in Temperatures from 1948")

# December
temp_dec <- get_month_vals(12, lat_adjust, lon_adjust, air.array, time_length)
plot_vals(12, temp_dec, "December Difference in Temperatures from 1948")

# Min and max years
years_jan_mar <- 1949:2022
years_apr_dec <- 1949:2021

# Find year of maximum temperature for each month that data was collected
max_years = rep()
max_years <- append(max_years, years_jan_mar[which.max(temp_jan)])
max_years <- append(max_years, years_jan_mar[which.max(temp_feb)])
max_years <- append(max_years, years_jan_mar[which.max(temp_mar)])
max_years <- append(max_years, years_apr_dec[which.max(temp_apr)])
max_years <- append(max_years, years_apr_dec[which.max(temp_may)])
max_years <- append(max_years, years_apr_dec[which.max(temp_jun)])
max_years <- append(max_years, years_apr_dec[which.max(temp_jul)])
max_years <- append(max_years, years_apr_dec[which.max(temp_aug)])
max_years <- append(max_years, years_apr_dec[which.max(temp_sep)])
max_years <- append(max_years, years_apr_dec[which.max(temp_oct)])
max_years <- append(max_years, years_apr_dec[which.max(temp_nov)])
max_years <- append(max_years, years_apr_dec[which.max(temp_dec)])

# Also use which.min to find minimum years
min_years = rep()
min_years <- append(min_years, years_jan_mar[which.min(temp_jan)])
min_years <- append(min_years, years_jan_mar[which.min(temp_feb)])
min_years <- append(min_years, years_jan_mar[which.min(temp_mar)])
min_years <- append(min_years, years_apr_dec[which.min(temp_apr)])
min_years <- append(min_years, years_apr_dec[which.min(temp_may)])
min_years <- append(min_years, years_apr_dec[which.min(temp_jun)])
min_years <- append(min_years, years_apr_dec[which.min(temp_jul)])
min_years <- append(min_years, years_apr_dec[which.min(temp_aug)])
min_years <- append(min_years, years_apr_dec[which.min(temp_sep)])
min_years <- append(min_years, years_apr_dec[which.min(temp_oct)])
min_years <- append(min_years, years_apr_dec[which.min(temp_nov)])
min_years <- append(min_years, years_apr_dec[which.min(temp_dec)])

gaps <- rep()
for (x in 1:12) {
  gaps <- append(gaps, max_years[x] - min_years[x])
}
plot(1:12, gaps, type = 'l', xlab = "month", ylab = "year difference", 
     main = "Years between maximum and minimum temperatures", col = 'red', lwd = 3)

df_years <- data.frame(1:12, max_years, min_years)
ggplot(df_years, aes(1:12)) +
  geom_line(aes(y=max_years), color="red", size = 3) +
  geom_line(aes(y=min_years), color="blue", size = 3) +
  xlab("Month") + ylab("Year") + ggtitle("Years of Maximum (Red) and Minimum (Blue) Temperatures")

# Regression

# Residual Plot

# Take year with most difference and 

# Heat maps depicting difference

# Sea ice dataset
nc_seaice <- nc_open("Z:/students/axie/project/G10010_V2/G10010_sibt1850_v2.0.nc")
lon <- ncvar_get(nc_seaice, "longitude")
lat <- ncvar_get(nc_seaice, "latitude", verbose = F)
t <- ncvar_get(nc_seaice, "time")

seaice.array <- ncvar_get(nc_seaice, "seaice_conc") # store the data in a 3-dimensional array
fillvalue <- ncatt_get(nc_seaice, "seaice_conc", "_FillValue")

nc_close(nc_mean_temp)

# check which longitude values have ice
seaice.slice <- seaice.array[, , 1]
lon_adjust <- 100
ice_vals <- rep()
for (x in 1:240) {
  ice_vals <- append(ice_vals, seaice.array[, , 1][lon_adjust, x])
}
plot(1:240, ice_vals)
