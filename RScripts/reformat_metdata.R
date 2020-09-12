library(readr)
library(tidyverse)
library(lubridate)
library(dplyr)
setwd("C:/Users/world/Desktop/Github/Climate/Climate_Data/Met_Stations/SCBI/ForestGEO_met_station-SCBI/Data")
years <- c(2011:2019)

for(i in years){
  name <- paste0("data", i)
  Filename <- paste("SCB_Metdata_5min", i, sep = "_")
  Filename <- paste(Filename, "csv", sep = ".")
  assign(name, read.csv(Filename, skip = 3))

}
#Problem: some temps are appearing as NaN (not a number)
names(data2014) <- names(data2011)
names(data2015) <- names(data2011)
names(data2016) <- names(data2011)
names(data2017) <- names(data2011)
names(data2018) <- names(data2011)
names(data2019) <- names(data2011)

data <- rbind(data2011,data2012,data2013,data2014,data2015,data2016,data2017,data2018,data2019) 
warnings()

names(data)[7] <- "Air_avgtmp1"
names(data)[8] <- "Air_std1"
names(data)[9] <- "Air_avgtmp2"
names(data)[10] <- "Air_std2"
data$Air_avgtmp1 <- as.character(data$Air_avgtmp1)
data$Air_avgtmp2 <- as.character(data$Air_avgtmp2)
data$Air_avgtmp1 <- as.numeric(data$Air_avgtmp1)
data$Air_avgtmp2<- as.numeric(data$Air_avgtmp2)
head(data)

data <- data %>%
  mutate(X = paste0(X, ":00"),
         X = parse_date_time(X, orders = "mdy HMS"),
         date = date(X), 
         time = chron::times(strftime(X,"%H:%M:%S", tz = "UTC")))


data <- data[complete.cases(data$Air_avgtmp2),]
#dailymeans1 <- aggregate(data$Air_avgtmp1, by = list(data$date), FUN = mean)
#dailymeans2 <- aggregate(data$Air_avgtmp2, by = list(data$date), FUN = mean)

dailymax1 <- aggregate(data$Air_avgtmp1, by = list(data$date), FUN = max)
dailymax2 <- aggregate(data$Air_avgtmp2, by = list(data$date), FUN = max)
dailymins1 <- aggregate(data$Air_avgtmp1, by = list(data$date), FUN = min)
dailymins2 <- aggregate(data$Air_avgtmp2, by = list(data$date), FUN = min)

#SKip following section if appending missing data
d1 <- separate(dailymax1, "Group.1", c("Year", "Month", "Day"), sep = "-")
d1 <- cbind(d1, dailymins1[,2])
d1$precip <- -99.9
d1 <- d1[,c(1,2,3,6,4,5)]
write.csv(d1, file = "SCBI_mettower_dailymeans_sensor1.csv", col.names = FALSE) #change name of file

d2 <- separate(dailymax2, "Group.1", c("Year", "Month", "Day"), sep = "-")
d2 <- cbind(d2, dailymins2[,2])
d2$precip <- -99.9
d2 <- d2[,c(1,2,3,6,4,5)]
write.csv(d2, file = "SCBI_mettower_dailymeans_sensor2.csv", col.names = FALSE)

#Run climpact to determine missing dates, then fill them with either -99.9 for NA or supplement with NCDC Front royal data

NCDC_NOAA_precip_temp <- read_csv("C:/Users/world/Desktop/Github/growth_phenology/climate data/NCDC_NOAA_precip_temp.csv")
missingdates <-read_csv("C:/Users/world/Desktop/Github/growth_phenology/climate data/SCBI_mettower_dailymeans_sensor1_missing_dates.csv")

missingdates$newdate <- strptime(as.character(missingdates$`06/01/2011`), "%d/%m/%Y")
missingdates$newdate <- format(missingdates$newdate, "%Y-%m-%d")

NCDC_NOAA_precip_temp$newdate <- strptime(as.character(NCDC_NOAA_precip_temp$DATE), "%d/%m/%Y")
NCDC_NOAA_precip_temp$newdate <- format(NCDC_NOAA_precip_temp$newdate, "%Y-%m-%d")

dailymax1$Group.1 <- as.Date(dailymax1$Group.1)

#dates in NCDC data
missingdates_data <- NCDC_NOAA_precip_temp[NCDC_NOAA_precip_temp$newdate %in% missingdates$newdate,]
#No dates in NCDC data
missingdates_nodata <- missingdates[!(missingdates$newdate %in% missingdates_data$newdate),]


missingdates_data_tmax <- missingdates_data[,c(15,9)]
missingdates_data_tmax$newdate <- as.Date(missingdates_data_tmax$newdate)

missingdates_data_tmin <- missingdates_data[,c(15,10)]
missingdates_data_tmin$newdate <- as.Date(missingdates_data_tmin$newdate)

names(dailymax1) <- names(missingdates_data_tmax)
names(dailymins1) <- names(missingdates_data_tmin)

#adding dates w/ data
dailymax1_missingadded <- full_join(missingdates_data_tmax,dailymax1)
dailymins1_missingadded <- full_join(missingdates_data_tmin,dailymins1)

#Adding no data dates
missingdates_nodata <- missingdates_nodata[,2]
missingdates_nodata$newdate <- as.Date(missingdates_nodata$newdate)
dailymax1_missingadded <- full_join(dailymax1_missingadded,missingdates_nodata)
dailymins1_missingadded <- full_join(dailymins1_missingadded,missingdates_nodata)

#Makes the csv files
d1 <- separate(dailymax1_missingadded, "newdate", c("Year", "Month", "Day"), sep = "-")
d1 <- cbind(d1, dailymins1_missingadded[,2]) #Use merge
d1$precip <- -99.9
d1 <- d1[,c(1,2,3,6,4,5)] #make sure years are in ascending order
write.csv(d1, file = "SCBI_mettower_data_sensor1.csv", col.names = FALSE, row.names = FALSE) #change name of file
