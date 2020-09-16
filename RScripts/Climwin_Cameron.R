#Climwin attempt
library(climwin)
library(readr)
library(lubridate)
library(tidyverse)
#NCDC_NOAA Front Royal station data
#climate <- read_csv("climate data/NCDC_NOAA_precip_temp.csv")
#climate <- climate[complete.cases(climate$TMAX),]
#climate <- climate[complete.cases(climate$DATE),]

#SCBI met tower
climate <- read_csv("climate data/SCBI_mettower_data_sensor2.csv", col_names = FALSE)
colnames(climate) <- c("year", "month", "day", "precip", "TMAX", "TMIN")
climate[climate$TMAX == -99.9,] <- NA
climate <- climate[complete.cases(climate$TMAX),]

climate$DATE <- paste(climate$day, climate$month, climate$year, sep = "/")
climate$DATE <- strptime(as.character(climate$DATE), format = "%d/%m/%Y")
climate$DATE <- format(climate$DATE, "%d/%m/%Y")

#The data containing the biological responses for testing
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V4.csv") #Master datafrmae containing 20%, 50%, and 75% growth milestones

rangedates <- data.frame("feb1", 30)
rangedates2 <- data.frame("mar1", 59)
names(rangedates) <- names(rangedates2)
rangedates <- rbind(rangedates, rangedates2)
colnames(rangedates) <- c("day", "doy")

for(w in unique(Wood_pheno_table$wood_type)){
  for(j in unique(Wood_pheno_table$perc)){
twentyfive <- subset(Wood_pheno_table, perc == j & wood_type == w) # subset to only 25%

#We would want to test all three percentages (25, 50, 75) but I dont know how to.
#I would also like to test how $Max_rate, $Max_rate_DOY, and $tot (total growth) are correlated with climate
biodata <- data.frame(NULL)
for(i in c(2011:2019)){ #Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
  df <- subset(twentyfive, year == i) #using twentyfive dataset
  df$date <- as.Date(df$DOY, origin = paste0(i, "-01-01"))
  df$date <- strftime(df$date, format = "%d/%m/%Y")
  biodata <- rbind(biodata, df)
}

refdateround <- round(mean(biodata$DOY))

#climatecheck <- climate[c(1:121),]
#biodata <- biodata[!(biodata$year == 2011),]
for (k in rangedates$doy) {

refdate <- data.frame(round(mean(biodata$DOY)))
refdate$round.mean.biodata.DOY.. <- as.Date(refdate$round.mean.biodata.DOY.., origin = paste0("2011-01-01"))
refdate <- separate(refdate, "round.mean.biodata.DOY..", c("Year", "Month", "Day"), sep = "-")

MassWin <- slidingwin(xvar = list(Temp = climate$TMAX),
                      cdate = climate$DATE,
                      bdate = biodata$date,
                      baseline = lm(DOY ~ 1, data = biodata),
                      cinterval = "day",
                      range = c((refdateround-k), 0), #feb 1st
                      type = "absolute", refday = c(refdate$Day, refdate$Month),
                      stat = "mean",
                      func = "lin",
                      cmissing = "method2")


MassRand <- randwin(repeats = 5,
                    xvar = list(Temp = climate$TMAX),
                    cdate = climate$DATE,
                    bdate = biodata$date,
                    baseline = lm(DOY ~ 1, data = biodata),
                    cinterval = "day",
                    range = c((refdateround-k), 0),
                    type = "absolute", refday = c(refdate$Day,refdate$Month),  #make reference day the mean DOY
                    stat = "mean",
                    func = "lin",
                    cmissing = "method2")
pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

MassSingle <- singlewin(xvar = list(Temp = climate$TMAX),
                        cdate = climate$DATE,
                        bdate = biodata$date,
                        baseline = lm(DOY ~ 1, data = biodata),
                        cinterval = "day",
                        range = c(MassWin[[1]][["Dataset"]][[1,2]], MassWin[[1]][["Dataset"]][[1,3]]),
                        type = "absolute", refday = c(refdate$Day,refdate$Month),
                        stat = "mean",
                        func = "lin",
                        cmissing = "method2")


MassOutput <- MassWin[[1]][["Dataset"]]
MassRand <- MassRand[[1]]
#plotdelta(dataset = MassOutput)
#plotweights(dataset = MassOutput)
#plothist(dataset = MassOutput, datasetrand = MassRand)
#plotbetas(dataset = MassOutput)
#plotwin(dataset = MassOutput)
#plotbest(dataset = MassOutput,
#         bestmodel = MassSingle$BestModel,
#         bestmodeldata = MassSingle$BestModelData)

png(filename = paste("SCBI", "mettower",w, k, j, ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300)
plotalloutput <- plotall(dataset = MassOutput,
        datasetrand = MassRand,
        bestmodel = MassSingle$BestModel,
        bestmodeldata = MassSingle$BestModelData)

dev.off()
}}}
