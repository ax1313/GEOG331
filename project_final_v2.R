# GEOG331 Project Script
# Andrew Xie
# 05/12/22

# Code adapted from: https://rpubs.com/boyerag/297592

library(ncdf4) 
library(raster) # Use for temperature data
library(rgdal)
library(ggplot2)

# Temperature (monthly mean) data (air.mon.mean.nc)
# Data starts from January 1, 1948 and ends on March 1, 2022

# load in file and longitude, latitude, and time values
nc_mean_temp <- nc_open('Z:/students/axie/project/air.mon.mean.nc')
lon <- ncvar_get(nc_mean_temp, "lon")
lat <- ncvar_get(nc_mean_temp, "lat", verbose = F)
t <- ncvar_get(nc_mean_temp, "time")

air.array <- ncvar_get(nc_mean_temp, "air") # store data in 3D array
fillvalue <- ncatt_get(nc_mean_temp, "air", "_FillValue")

nc_close(nc_mean_temp)

time_length = nc_mean_temp[["dim"]][["time"]][["len"]]

get_month_vals <- function(first_month_val, lat_val, lon_val, array, length) {
  new_lon_val = round((180 - lon_val) / 2.5) + 1 # Get index value of longitude to extract from air.diff
  second_month_val <- first_month_val + 12 # Start measurements from next year but with same calendar month
  air.first <- array[, , first_month_val]
  temp <- rep()
  while (second_month_val <= length) {
    air.last <- air.array[, , second_month_val]
    air.diff <- air.last - air.first # Difference between two months
    temp <- append(temp, air.diff[new_lon_val, lat_val])
    second_month_val <- second_month_val + 12 # Keep calendar month consistent
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

# Arctic Coordinates
lat_adjust = 6 # Index value corresponding to closest latitude
lon_adjust = 100.1140

# # North Pole
# lat_adjust <- 1
# lon_adjust <-  135

# # Hamilton
# lat_adjust <- 20
# lon_adjust <-  75.5446

# # Miami
# lat_adjust <- 27
# lon_adjust <- 80.1918

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

# October From 1950 (Get rid of high temperature values from 1948 and 1949 for Arctic)
temp_oct_1950 <- get_month_vals(34, lat_adjust, lon_adjust, air.array, time_length)
plot(1951:2021, temp_oct_1950, xlab = "year", ylab = "temp difference (degC)", main = "October Difference in Temperatures from 1950")

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
max_years <- rep()
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
min_years <- rep()
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

# Max-min year plot
df_years <- data.frame(1:12, max_years, min_years)
ggplot(df_years, aes(1:12)) +
  geom_line(aes(y=max_years), color="red", size = 3) +
  geom_line(aes(y=min_years), color="blue", size = 3) +
  xlab("Month") + ylab("Year") + ggtitle("Years of Maximum (Red) and Minimum (Blue) Temperatures")

# Get standard deviation of temperature values for each month
# The higher the standard deviation, the more varied the temperature differences are
sd_temps <- rep()
sd_temps <- append(sd_temps, sd(temp_jan))
sd_temps <- append(sd_temps, sd(temp_feb))
sd_temps <- append(sd_temps, sd(temp_mar))
sd_temps <- append(sd_temps, sd(temp_apr))
sd_temps <- append(sd_temps, sd(temp_may))
sd_temps <- append(sd_temps, sd(temp_jun))
sd_temps <- append(sd_temps, sd(temp_jul))
sd_temps <- append(sd_temps, sd(temp_aug))
sd_temps <- append(sd_temps, sd(temp_sep))
sd_temps <- append(sd_temps, sd(temp_oct))
sd_temps <- append(sd_temps, sd(temp_nov))
sd_temps <- append(sd_temps, sd(temp_dec))
# Plot standard deviation vs. month
plot(1:12, sd_temps, type = 'l', xlab = "Month", ylab = "Standard Deviation (degC)",
     main = "Temperature Standard Deviation over 12 Months", col = 'blue', lwd = '3')

# Regression analysis for October
years_1951 <- 1951:2021
fit_oct_1950 <- lm(temp_oct_1950 ~ years_1951)
summary(fit_oct_1950)

# Sea ice dataset
nc_seaice <- nc_open("Z:/students/axie/project/G10010_V2/G10010_sibt1850_v2.0.nc")
lon <- ncvar_get(nc_seaice, "longitude")
lat <- ncvar_get(nc_seaice, "latitude", verbose = F)
t <- ncvar_get(nc_seaice, "time")

seaice.array <- ncvar_get(nc_seaice, "seaice_conc") # store data in 3D array
fillvalue <- ncatt_get(nc_seaice, "seaice_conc", "_FillValue")

nc_close(nc_mean_temp)

# check which longitude values have ice
seaice.slice <- seaice.array[, , 2000] # Adjust third index to see patterns in amount of seaice
lon_adjust <- 100 # Pick constant longitude value
ice_vals <- rep()
for (x in 1:96) { # 96th index corresponds to 66.125 degrees North latitude
  ice_vals <- append(ice_vals, seaice.slice[lon_adjust, x])
}
plot(nc_seaice[["dim"]][["latitude"]][["vals"]][1:96], ice_vals, xlab = 'Latitude (degrees)',
     ylab = 'Sea-Ice Concentration', main = 'Sea-Ice Concentration')
