# GEOG331 Activity 3 Script
# Andrew Xie
# 03/04/22

library(lubridate)

#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
}

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("data/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("data/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

# Start QUESTION 5 Here
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
assert(all(datW$DD[lightscale > 0] %in% datW$DD), "Error: Cannot subset datW w/ lightscale")
#End QUESTION 5 Here

# Start QUESTION 6 Here
# Make initial plot to detect suspect wind speed values
plot(datW$DD, datW$wind.speed, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind speed (m/s)")

# Eliminate extraneous wind speed values
datW$wind.speedQ1 <- ifelse(datW$wind.speed > 0.7 & datW$DD < 200, NA,
                                 ifelse(datW$wind.speed > 1.2 & datW$DD > 200, NA, 
                                 ifelse(datW$wind.speed < 0.5 & datW$DD > 205, NA, datW$wind.speed)))

# Check if new temp wind speed column is not the same as the original wind speed column
assert(all(is.na(datW$wind.speedQ1[(datW$wind.speed > 0.7 & datW$DD < 200) | (datW$wind.speed > 1.2 & datW$DD > 200) | (datW$wind.speed < 0.5 & datW$DD > 205)])), "Error: Data not filtered")

# Make line / point plot (Day of Year vs. Wind Speed)
plot(datW$DD, datW$wind.speedQ1, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind speed (m/s)")
# End QUESTION 6 Here

# Start QUESTION 7 Here
# Make 4 plots at a time
par(mfrow = c(2,2))

# Generate four plots
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil temp (degrees C)")

plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (m^3 water / m^3 soil)")

# Precipitation contains a few extreme values to be removed
datW$precipitationQ1 <- ifelse(datW$precipitation > 6, NA, datW$precipitation)

plot(datW$DD, datW$precipitationQ1, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (mm)")

plot(datW$DD, datW$air.tempQ1, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
# End QUESTION 7 Here

# Start QUESTION 8 Here
num_calc = length(datW$air.tempQ1) + length(datW$wind.speedQ1) + length(datW$precipitationQ1)
  length(datW$soil.moisture[!is.na(datW$soil.moisture)]) + length(datW$soil.temp[!is.na(datW$soil.temp)])

# Get earliest and latest days data was collected
# For soil, latest day is in mid-July due to the sensor malfunction
min_day <- min(datW$timestamp)
max_day_soil <- max(datW$timestamp[!is.na(datW$soil.moisture)])
max_day <- max(datW$timestamp[is.na(datW$soil.moisture)])

# Find average air temperature (Accuracy = +/- 0.6 degrees C)
avg_air_temp <- mean(datW$air.tempQ1, na.rm = TRUE)

#Find average wind speed (Accuracy = The greater of 0.3 m/s or 3% of measurement)
avg_wind_speed <- mean(datW$wind.speedQ1, na.rm = TRUE)
# Check accuracy
accuracy_ws <- 0.03 * avg_wind_speed

# Find average soil moisture (Accuracy = +/- 0.03 m^3/m^3)
avg_soil_mt <- mean(datW$soil.moisture, na.rm = TRUE)

# Find average soil temperature (Accuracy = +/- 1 m)
avg_soil_temp <- mean(datW$soil.temp, na.rm = TRUE)

# Find total precipitation (Accuracy = +/- 5% of measurement from 0 to 50 mm/h)
total_precip <- sum(datW$precipitationQ1, na.rm = TRUE)
# Check accuracy
accuracy_precip <- 0.05 * total_precip

# End QUESTION 8 Here

# Start QUESTION 9 Here
# Make 4 plots at a time
par(mfrow = c(2,2))

# Generate four plots
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (m^3 water / m^3 soil)")

plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil temp (degrees C)")

plot(datW$DD, datW$precipitationQ1, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (mm)")

plot(datW$DD, datW$air.tempQ1, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
# End QUESTION 9 Here
