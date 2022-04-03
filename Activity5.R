# GEOG331 Activity 5 Script
# Andrew Xie
# 04/03/2022

# SECTION 1: Working with USGS streamflow data

#load in lubridate and ggplot
library(lubridate)
library(ggplot2)

#read in streamflow data
datH <- read.csv("data/streamflow/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("data/streamflow/2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365)) 

# SECTION 2: Basic plot formatting

# Start QUESTION 5 Here
#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       

axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

# Aggregate over 2017 data to find average discharge values and add them to new dataframe
aveF17 <- aggregate(datD$discharge[datD$year == '2017'], by=list(datD$doy[datD$year == '2017']), FUN="mean")
colnames(aveF17) <- c("doy","dailyAve")

# Plot red line with 2017 average data over previous graph
lines(aveF17$doy, aveF17$dailyAve, col = "red")
axis(1, c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 366), #tick intervals (by month)
     # Each tick interval is located at the approximate end of each month
     lab=seq(0,12, by=1)) #tick labels
# End QUESTION 5 Here

# SECTION 3: Making a hydrograph

# Start QUESTION 7 Here
# Get first and last years of data in datP
minYear <- min(datP$year)
maxYear <- max(datP$year)

# Create new data frame
df7 <- data.frame(matrix(ncol = 2, nrow = 0))

for (year in minYear:maxYear) {
  # Assign number of days based on whether year is a leap year or not
  num_days = 365
  if (leap_year(year)) {
    num_days = 366
  }
  # for each year, add days with measurements for all 24 hours into new data frame df7
  for (day in 1:num_days) {
    if (length(datP$hour[datP$year == year & datP$doy == day]) == 24) {
      df7 <- rbind(df7, c(day, year))
    }
  }
}
# set names of columns in df7
colnames(df7) <- c('doy', 'year')

# calculate decYear and add to df7
df7$decYear <- ifelse(leap_year(df7$year),df7$year + (df7$doy/366),
                      df7$year + (df7$doy/365))

# plot previous discharge graph and place points above dates in df7
dev.new(width=8,height=8)
plot(datD$decYear[datD$year <= maxYear & datD$year >= minYear], datD$discharge[datD$year <= maxYear & datD$year >= minYear], type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
points(df7$decYear, rep(200, nrow(df7)), pch = 1)
# End QUESTION 7 Here

# Start QUESTION 8 Here
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 12 & datD$doy < 15 & datD$year == 2012,]
hydroP <- datP[datP$doy >= 12 & datP$doy < 15 & datP$year == 2012,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

#start new plot
dev.new(width=8,height=8)

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}
# End QUESTION 8 Here

# SECTION 4: Making box plots and violin plots

# Start QUESTION 9 Here
# Assign number of days based on whether year is a leap year or not
num_days = 365
if (leap_year(year)) {
  num_days = 366
}

# Assign season to dates
# Approximate date ranges:
# January 1-March 20 and December 21-31: Winter
# March 21 - June 20: Spring
# June 21 - September 20: Summer
# September 21 - December 20: Autumn
datD$season <- ifelse((datD$decDay/num_days) < (80/num_days), 'Winter',
                      ifelse((datD$decDay/num_days) < (172/num_days), 'Spring',
                             ifelse((datD$decDay/num_days) < (264/num_days), 'Summer',
                             ifelse((datD$decDay/num_days) < (355/num_days), 'Autumn', 'Winter'))))

# subset data by year
datD2016 <- subset(datD, year == 2016)
datD2017 <- subset(datD, year == 2017)

# Order seasons so that they show up in order on the violin plot
datD2016$season <- factor(datD2016$season, levels = c("Winter", "Spring", "Summer", "Autumn"))
datD2017$season <- factor(datD2017$season, levels = c("Winter", "Spring", "Summer", "Autumn"))

# Make violin plots for 2016 and 2017 by season with centered titles
dev.new(width=8,height=8)
ggplot(data=datD2016, aes(season, discharge)) +
  geom_violin() + ggtitle("2016 discharge") +
  theme(plot.title = element_text(hjust = 0.5))

dev.new(width=8,height=8)
ggplot(data=datD2017, aes(season, discharge)) +
  geom_violin() + ggtitle("2017 discharge") + 
  theme(plot.title = element_text(hjust = 0.5))
# End QUESTION 9 Here
