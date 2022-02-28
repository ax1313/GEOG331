# GEOG331 Activity 3 Script
# Andrew Xie
# 02/28/22

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

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusing issues.
#It can be particularly confusing when you are just learning R.
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

# Start QUESTION 5 Here
assert(all(datW$DD[lightscale > 0] %in% datW$DD), "Error: Cannot subset datW w/ lightscale")
#End QUESTION 5 Here

# Start QUESTION 6 Here
# Make initial plot to detect suspect wind speed values
plot(datW$DD, datW$wind.speed, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind speed (m/s)")

# Eliminate extraneous wind speed values
datW$wind.speed.tempQ1 <- ifelse(datW$wind.speed > 0.7 & datW$DD < 200, NA,
                                 ifelse(datW$wind.speed > 1.2 & datW$DD > 200, NA, 
                                 ifelse(datW$wind.speed < 0.5 & datW$DD > 205, NA, datW$wind.speed)))

# Check if new temp wind speed column is not the same as the original wind speed column
assert(!setequal(datW$wind.speed, datW$wind.speed.tempQ1), "Error: Data not filtered")

# Make line / point plot (Day of Year vs. Wind Speed)
plot(datW$DD, datW$wind.speed.tempQ1, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind speed (m/s)")
# End QUESTION 6 Here

# Start QUESTION 7 Here
# Make 4 plots at a time
par(mfrow = c(2,2))

# Generate four plots
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil temp (degrees C)")

plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (degrees C)")

# Precipitation contains a few extreme values to be removed
datW$precipitation.tempQ1 <- ifelse(datW$precipitation > 6, NA, datW$precipitation)

plot(datW$DD, datW$precipitation.tempQ1, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (mm)")

plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
# End QUESTION 7 Here

# Start QUESTION 8 Here
num_calc = length(datW$air.temperature[!is.na(datW$soil.moisture)]) + length(datW$precipitation[!is.na(datW$soil.moisture)]) +
  length(datW$soil.moisture[!is.na(datW$soil.moisture)]) + length(datW$soil.temp[!is.na(datW$soil.temp)])

min_day = min(datW$timestamp)
max_day = max(datW$timestamp[!is.na(datW$soil.moisture)])

# End QUESTION 8 Here

# Start QUESTION 9 Here
# Make 4 plots at a time
par(mfrow = c(2,2))

# Generate four plots
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (degrees C)")

plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil temp (degrees C)")

plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (mm)")

plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
# End QUESTION 9 Here
