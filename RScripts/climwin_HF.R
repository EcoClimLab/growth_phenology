#Climwin analysis script
library(climwin)
library(readr)
library(lubridate)
library(tidyverse)
library(dplyr)
#Merge two weather DF's into one ----
#HF_1964to2002 <- read_csv("climate data/HF_1964to2002.csv")
#HF_2001toPresent <- read_csv("climate data/HF_2001toPresent.csv")
#
#HF_1964to2002$DOY <- yday(HF_1964to2002$date)# format = "%y-%m-%d")
#
#HF_1964to2002$year <- as.numeric(format(HF_1964to2002$date, format = "%Y"))
#HF_1998to2002 <- subset(HF_1964to2002, year >=1998)
#HF_1998to2002 <- HF_1998to2002[,c(1,4,11,12)]
#HF_1998to2002$month <- as.numeric(format(HF_1998to2002$date, format = "%m"))
#HF_1998to2002$day <- as.numeric(format(HF_1998to2002$date, format = "%d"))
#
#HF_2001toPresent$DOY <- yday(HF_2001toPresent$date)
#HF_2001toPresent$year <- as.numeric(format(HF_2001toPresent$date, format = "%Y"))
#HF_2001toPresent <- HF_2001toPresent[,c(1,5,47,48)]
#HF_2001toPresent <- HF_2001toPresent[c(-1:-505),]
#HF_2001toPresent <- subset(HF_2001toPresent, year == 2002 | year == 2003)
#HF_2001toPresent$month <- as.numeric(format(HF_2001toPresent$date, format = "%m"))
#HF_2001toPresent$day <- as.numeric(format(HF_2001toPresent$date, format = "%d"))
#
#HF_weatherdata <- rbind(HF_1998to2002, HF_2001toPresent)
#
#write.csv(HF_weatherdata, file = "HF_weatherdata.csv", row.names = FALSE)
####Read in climate and biological data for climwin analysis ----
#SCBI met tower
#climate <- read_csv("climate data/SCBI_mettower_data_sensor2.csv", col_names = FALSE) #only goes to October 2019, fix?
climate <- read_csv("climate data/HF_weatherdata.csv", col_names = TRUE) #only goes to October 2019, fix?

#colnames(climate) <- c("year","month", "day", "prcip", "TMAX", "TMIN")

colnames(climate) <- c("date","TMAX", "doy", "year", "month", "day")
#climate[climate$TMAX == -99.9,] <- NA

climate <- climate[complete.cases(climate$TMAX),]

climate$DATE <- paste(climate$day, climate$month, climate$year, sep = "/")
climate$DATE <- strptime(as.character(climate$DATE), format = "%d/%m/%Y")
climate$DATE <- format(climate$DATE, "%d/%m/%Y")

#The data containing the biological responses for testing
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_HarvardForest_V5CLEAN.csv") #Master datafrmae containing 20%, 50%, and 75% growth milestones

####Percentage DOY climwin all wood types, all percs DAILY ----
#rangedates <- data.frame("feb1", round(30)) #/7 for week
#rangedates2 <- data.frame("mar1", round(59)) #/7 for week
#names(rangedates) <- names(rangedates2)
#rangedates <- rbind(rangedates, rangedates2)
#colnames(rangedates) <- c("day", "doy")

dffinal <- data.frame(1,1,1,1,1,1,1,1,1,1)
for(w in unique(Wood_pheno_table$wood_type)){
  for(j in unique(Wood_pheno_table$perc)){
    twentyfive <- subset(Wood_pheno_table,  wood_type == w & perc == j ) #

    biodata <- data.frame(NULL)
    for(i in c(1998:203)){ #Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
      df <- subset(twentyfive, year == i) #using twentyfive dataset
      df$date <- as.Date(df$DOY, origin = paste0(i, "-01-01"))
      df$date <- strftime(df$date, format = "%d/%m/%Y")
      biodata <- rbind(biodata, df)
    }

    refdateround <- round(mean(biodata$DOY)) #/7 for week

    #for (k in rangedates$doy) {

    refdate <- data.frame(round(mean(biodata$DOY)))
    refdate$round.mean.biodata.DOY.. <- as.Date(refdate$round.mean.biodata.DOY.., origin = paste0("1998-01-01"))
    refdate <- separate(refdate, "round.mean.biodata.DOY..", c("Year", "Month", "Day"), sep = "-")

    MassWin <- slidingwin(xvar = list(Temp = climate$TMAX),
                          cdate = climate$DATE,
                          bdate = biodata$date,
                          baseline = lm(DOY ~ 1, data = biodata),
                          cinterval = "day",
                          range = c((refdateround-30), 0),
                          type = "absolute", refday = c(as.numeric(refdate$Day), as.numeric(refdate$Month)),
                          stat = "mean",
                          func = "lin",
                          cmissing = "method2")

    MassRand <- randwin(repeats = 1,
                        xvar = list(Temp = climate$TMAX),
                        cdate = climate$DATE,
                        bdate = biodata$date,
                        baseline = lm(DOY ~ 1, data = biodata),
                        cinterval = "day",
                        range = c((refdateround-30), 0),
                        type = "absolute", refday = c(as.numeric(refdate$Day),as.numeric(refdate$Month)),
                        stat = "mean",
                        func = "lin",
                        cmissing = "method2")
    pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

    MassOutput <- MassWin[[1]][["Dataset"]]
    MassRand <- MassRand[[1]]
    #windowopen <- as.Date(refdateround- MassWin[[1]][["Dataset"]][[1,2]] , origin = paste0("2011-01-01"))
    #windowclose <- as.Date(refdateround - MassWin[[1]][["Dataset"]][[1,3]] , origin = paste0("2011-01-01"))

    winmedians <- medwin(MassOutput, cw = 0.95)
    medianwindowopen <- as.Date(refdateround- winmedians[["Median Window Open"]] , origin = paste0("2011-01-01"))
    medianwindowclose <- as.Date(refdateround - winmedians[["Median Window Close"]] , origin = paste0("2011-01-01"))

    df <- data.frame(w, j, round(mean(biodata$DOY)), refdate$Month, refdate$Day,MassWin[[1]][["Dataset"]][[1,2]], MassWin[[1]][["Dataset"]][[1,3]],MassOutput[1,4],as.character(medianwindowopen),as.character(medianwindowclose))#add w,#add/7
    names(dffinal) <- names(df)
    dffinal <- rbind(dffinal, df)
    png(filename = paste("HF", j ,w,  ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300) #add w
    plotalloutput <- plotall(dataset = MassOutput,
                             datasetrand = MassRand,
                             bestmodel = MassWin[[1]]$BestModel,
                             bestmodeldata = MassWin[[1]]$BestModelData)

    dev.off()
  }}#}
dffinal <- dffinal[-1,]
names(dffinal) <- c("wood_type", "percs", "refdoy", "refmonth", "refday", "winopendoy", "winclosedoy", "bestmodel_beta","median_winopendate","median_windowclosedate")
write.csv(dffinal, file = "daily_climwin_results.csv", row.names = FALSE)

########Percentage DOY climwin all wood types, all percs WEEKLY ----
rangedates <- data.frame("feb1", round(30/7)) #/7 for week
rangedates2 <- data.frame("mar1", round(59/7)) #/7 for week
names(rangedates) <- names(rangedates2)
rangedates <- rbind(rangedates, rangedates2)
colnames(rangedates) <- c("day", "doy")

dffinal <- data.frame(1,1,1,1,1,1,1,1,1,1)
for(w in unique(Wood_pheno_table$wood_type)){
  for(j in unique(Wood_pheno_table$perc)){
    twentyfive <- subset(Wood_pheno_table, wood_type == w & perc == j )#

    biodata <- data.frame(NULL)
    for(i in c(1998:2003)){ #Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
      df <- subset(twentyfive, year == i) #using twentyfive dataset
      df$date <- as.Date(df$DOY, origin = paste0(i, "-01-01"))
      df$date <- strftime(df$date, format = "%d/%m/%Y")
      biodata <- rbind(biodata, df)
    }

    refdateround <- round(mean(biodata$DOY)/7) #/7 for week

    #for (k in rangedates$doy) {

    refdate <- data.frame(round(mean(biodata$DOY)))
    refdate$round.mean.biodata.DOY.. <- as.Date(refdate$round.mean.biodata.DOY.., origin = paste0("1998-01-01"))
    refdate <- separate(refdate, "round.mean.biodata.DOY..", c("Year", "Month", "Day"), sep = "-")

    MassWin <- slidingwin(xvar = list(Temp = climate$TMAX),
                          cdate = climate$DATE,
                          bdate = biodata$date,
                          baseline = lm(DOY ~ 1, data = biodata),
                          cinterval = "week",
                          range = c((refdateround-(30/7)), 0),
                          type = "absolute", refday = c(refdate$Day, refdate$Month),
                          stat = "mean",
                          func = "lin",
                          cmissing = "method2")

    MassRand <- randwin(repeats = 5,
                        xvar = list(Temp = climate$TMAX),
                        cdate = climate$DATE,
                        bdate = biodata$date,
                        baseline = lm(DOY ~ 1, data = biodata),
                        cinterval = "week",
                        range = c((refdateround-(30/7)), 0),
                        type = "absolute", refday = c(refdate$Day,refdate$Month),
                        stat = "mean",
                        func = "lin",
                        cmissing = "method2")
    #pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

    MassOutput <- MassWin[[1]][["Dataset"]]
    MassRand <- MassRand[[1]]
    #windowopen <- as.Date((refdateround*7)- (MassWin[[1]][["Dataset"]][[1,2]]*7) , origin = paste0("2011-01-01"))
    #windowclose <- as.Date((refdateround*7) - (MassWin[[1]][["Dataset"]][[1,3]]*7) , origin = paste0("2011-01-01"))
    tryCatch(
    winmedians <- medwin(MassOutput, cw = 0.95),
    error = function(hi){
      winmedians <<- medwin(MassOutput, cw = .9999)
    }
    )
    medianwindowopen <- as.Date(refdateround*7- winmedians[["Median Window Open"]]*7 , origin = paste0("2011-01-01"))
    medianwindowclose <- as.Date(refdateround*7 - winmedians[["Median Window Close"]]*7 , origin = paste0("2011-01-01"))

    df <- data.frame(w,j, round(mean(biodata$DOY)/7),refdate$Month, refdate$Day,MassWin[[1]][["Dataset"]][[1,2]], MassWin[[1]][["Dataset"]][[1,3]],MassOutput[1,4],as.character(medianwindowopen), as.character(medianwindowclose))#add w, #add/7
    names(dffinal) <- names(df)
    dffinal <- rbind(dffinal, df)
    png(filename = paste("HF",w,j,  ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300) #add w
    plotalloutput <- plotall(dataset = MassOutput,
                             datasetrand = MassRand,
                             bestmodel = MassWin[[1]]$BestModel,
                             bestmodeldata = MassWin[[1]]$BestModelData)

    dev.off()
  }}#}
dffinal <- dffinal[-1,]
names(dffinal) <- c("wood_type", "percs", "refwoy","refmonth", "refday", "winopenwoy", "winclosewoy","bestmodel_beta","median_windowopendate", "median_windowclosedate")
write.csv(dffinal, file = "weekly_climwin_results_all_HF.csv", row.names = FALSE)


####Total growth DAILY ----
#rangedates <- data.frame("feb1", round(30)) #/7 for week
#rangedates2 <- data.frame("mar1", round(59)) #/7 for week
#names(rangedates) <- names(rangedates2)
#rangedates <- rbind(rangedates, rangedates2)
#colnames(rangedates) <- c("day", "doy")

dffinal <- data.frame(1,1,1,1,1,1,1,1,1)
for(w in unique(Wood_pheno_table$wood_type)){
  #for(j in unique(Wood_pheno_table$perc)){
  twentyfive <- subset(Wood_pheno_table, wood_type == w)# & perc == j )

  biodata <- data.frame(NULL)
  for(i in c(2011:2019)){ #Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
    df <- subset(twentyfive, year == i) #using twentyfive dataset
    df$date <- as.Date(df$DOY, origin = paste0(i, "-01-01"))
    df$date <- strftime(df$date, format = "%d/%m/%Y")
    biodata <- rbind(biodata, df)
  }

  refdateround <- round(mean(biodata$DOY)) #/7 for week

  #for (k in rangedates$doy) {

  refdate <- data.frame(round(mean(biodata$DOY)))
  refdate$round.mean.biodata.DOY.. <- as.Date(refdate$round.mean.biodata.DOY.., origin = paste0("2011-01-01"))
  refdate <- separate(refdate, "round.mean.biodata.DOY..", c("Year", "Month", "Day"), sep = "-")

  MassWin <- slidingwin(xvar = list(Temp = climate$TMAX),
                        cdate = climate$DATE,
                        bdate = biodata$date,
                        baseline = lm(tot ~ 1, data = biodata),
                        cinterval = "day",
                        range = c((refdateround-30), 0),
                        type = "absolute", refday = c(refdate$Day, refdate$Month),
                        stat = "mean",
                        func = "lin",
                        cmissing = "method2")

  MassRand <- randwin(repeats = 5,
                      xvar = list(Temp = climate$TMAX),
                      cdate = climate$DATE,
                      bdate = biodata$date,
                      baseline = lm(tot ~ 1, data = biodata),
                      cinterval = "day",
                      range = c((refdateround-30), 0),
                      type = "absolute", refday = c(refdate$Day,refdate$Month),
                      stat = "mean",
                      func = "lin",
                      cmissing = "method2")
  pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

  MassOutput <- MassWin[[1]][["Dataset"]]
  MassRand <- MassRand[[1]]
  #windowopen <- as.Date(refdateround- MassWin[[1]][["Dataset"]][[1,2]] , origin = paste0("2011-01-01"))
  #windowclose <- as.Date(refdateround - MassWin[[1]][["Dataset"]][[1,3]] , origin = paste0("2011-01-01"))
  winmedians <- medwin(MassOutput, cw = 0.95)
  medianwindowopen <- as.Date(refdateround- winmedians[["Median Window Open"]] , origin = paste0("2011-01-01"))
  medianwindowclose <- as.Date(refdateround - winmedians[["Median Window Close"]] , origin = paste0("2011-01-01"))

  df <- data.frame(w, round(mean(biodata$DOY)),refdate$Month, refdate$Day,MassWin[[1]][["Dataset"]][[1,2]], MassWin[[1]][["Dataset"]][[1,3]],MassOutput[1,4],as.character(medianwindowopen), as.character(medianwindowclose))#add j #add/7
  names(dffinal) <- names(df)
  dffinal <- rbind(dffinal, df)
  png(filename = paste("SCBI", "mettower","total_growth",w,  ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300) #add w
  plotalloutput <- plotall(dataset = MassOutput,
                           datasetrand = MassRand,
                           bestmodel = MassWin[[1]]$BestModel,
                           bestmodeldata = MassWin[[1]]$BestModelData)

  dev.off()
}#}#}
dffinal <- dffinal[-1,]
names(dffinal) <- c("wood_type", "refdoy", "refmonth", "refday","winopendoy", "winclosedoy","bestmodel_beta", "median_windowopendate","median_windowclosedate")
write.csv(dffinal, file = "daily_climwin_results_totalgrowth.csv", row.names = FALSE)

####Total growth WEEKLY ----
rangedates <- data.frame("feb1", round(30/7)) #/7 for week
rangedates2 <- data.frame("mar1", round(59/7)) #/7 for week
names(rangedates) <- names(rangedates2)
rangedates <- rbind(rangedates, rangedates2)
colnames(rangedates) <- c("day", "doy")

dffinal <- data.frame(1,1,1,1,1,1,1,1,1)
for(w in unique(Wood_pheno_table$wood_type)){
  #for(j in unique(Wood_pheno_table$perc)){
  twentyfive <- subset(Wood_pheno_table, wood_type == w) #& perc == j )

  biodata <- data.frame(NULL)
  for(i in c(1998:2003)){ #Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
    df <- subset(twentyfive, year == i) #using twentyfive dataset
    df$date <- as.Date(df$DOY, origin = paste0(i, "-01-01"))
    df$date <- strftime(df$date, format = "%d/%m/%Y")
    biodata <- rbind(biodata, df)
  }

  refdateround <- round(mean(biodata$DOY)/7) #/7 for week

  #for (k in rangedates$doy) {

  refdate <- data.frame(round(mean(biodata$DOY)))
  refdate$round.mean.biodata.DOY.. <- as.Date(refdate$round.mean.biodata.DOY.., origin = paste0("2011-01-01"))
  refdate <- separate(refdate, "round.mean.biodata.DOY..", c("Year", "Month", "Day"), sep = "-")

  MassWin <- slidingwin(xvar = list(Temp = climate$TMAX),
                        cdate = climate$DATE,
                        bdate = biodata$date,
                        baseline = lm(tot ~ 1, data = biodata),
                        cinterval = "week",
                        range = c((refdateround-(30/7)), 0),
                        type = "absolute", refday = c(refdate$Day, refdate$Month),
                        stat = "mean",
                        func = "lin",
                        cmissing = "method2")

  MassRand <- randwin(repeats = 5,
                      xvar = list(Temp = climate$TMAX),
                      cdate = climate$DATE,
                      bdate = biodata$date,
                      baseline = lm(tot ~ 1, data = biodata),
                      cinterval = "week",
                      range = c((refdateround-(30/7)), 0),
                      type = "absolute", refday = c(refdate$Day,refdate$Month),
                      stat = "mean",
                      func = "lin",
                      cmissing = "method2")
  pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

  MassOutput <- MassWin[[1]][["Dataset"]]
  MassRand <- MassRand[[1]]
  #windowopen <- as.Date(refdateround- MassWin[[1]][["Dataset"]][[1,2]] , origin = paste0("2011-01-01"))
  #windowclose <- as.Date(refdateround - MassWin[[1]][["Dataset"]][[1,3]] , origin = paste0("2011-01-01"))
  winmedians <- medwin(MassOutput, cw = 0.95)
  medianwindowopen <- as.Date(refdateround*7- winmedians[["Median Window Open"]]*7 , origin = paste0("2011-01-01"))
  medianwindowclose <- as.Date(refdateround*7 - winmedians[["Median Window Close"]]*7 , origin = paste0("2011-01-01"))

  df <- data.frame( w,round(mean(biodata$DOY)/7),refdate$Month, refdate$Day,MassWin[[1]][["Dataset"]][[1,2]], MassWin[[1]][["Dataset"]][[1,3]],MassOutput[1,4],as.character(medianwindowopen), as.character(medianwindowclose))#add w, #add/7
  names(dffinal) <- names(df)
  dffinal <- rbind(dffinal, df)
  png(filename = paste("SCBI", "mettower",w,"total_growth",  ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300) #add w
  plotalloutput <- plotall(dataset = MassOutput,
                           datasetrand = MassRand,
                           bestmodel = MassWin[[1]]$BestModel,
                           bestmodeldata = MassWin[[1]]$BestModelData)

  dev.off()
}#}
dffinal <- dffinal[-1,]
names(dffinal) <- c("wood_type", "refdoy", "refmonth", "refday","winopendoy", "winclosedoy", "bestmodel_beta","median_windowopendate", "median_windowclosedate")
write.csv(dffinal, file = "weekly_climwin_results_totalgrowth_all.csv", row.names = FALSE)
#

####Max rate DOY daily ----
rangedates <- data.frame("feb1", round(30)) #/7 for week
rangedates2 <- data.frame("mar1", round(59)) #/7 for week
names(rangedates) <- names(rangedates2)
rangedates <- rbind(rangedates, rangedates2)
colnames(rangedates) <- c("day", "doy")

dffinal <- data.frame(1,1,1,1,1,1,1,1,1)
for(w in unique(Wood_pheno_table$wood_type)){
  #for(j in unique(Wood_pheno_table$perc)){
  twentyfive <- subset(Wood_pheno_table, wood_type == w)# & perc == j )

  biodata <- data.frame(NULL)
  for(i in c(2011:2019)){ #Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
    df <- subset(twentyfive, year == i) #using twentyfive dataset
    df$date <- as.Date(df$max_rate_DOY, origin = paste0(i, "-01-01"))
    df$date <- strftime(df$date, format = "%d/%m/%Y")
    biodata <- rbind(biodata, df)
  }

  refdateround <- round(mean(biodata$max_rate_DOY)) #/7 for week

  #for (k in rangedates$doy) {

  refdate <- data.frame(round(mean(biodata$max_rate_DOY)))
  refdate$round.mean.biodata.max_rate_DOY.. <- as.Date(refdate$round.mean.biodata.max_rate_DOY.., origin = paste0("2011-01-01"))
  refdate <- separate(refdate, "round.mean.biodata.max_rate_DOY..", c("Year", "Month", "Day"), sep = "-")

  MassWin <- slidingwin(xvar = list(Temp = climate$TMAX),
                        cdate = climate$DATE,
                        bdate = biodata$date,
                        baseline = lm(max_rate_DOY ~ 1, data = biodata),
                        cinterval = "day",
                        range = c((refdateround-30), 0),
                        type = "absolute", refday = c(refdate$Day, refdate$Month),
                        stat = "mean",
                        func = "lin",
                        cmissing = "method2")

  MassRand <- randwin(repeats = 1,
                      xvar = list(Temp = climate$TMAX),
                      cdate = climate$DATE,
                      bdate = biodata$date,
                      baseline = lm(max_rate_DOY ~ 1, data = biodata),
                      cinterval = "day",
                      range = c((refdateround-30), 0),
                      type = "absolute", refday = c(refdate$Day,refdate$Month),
                      stat = "mean",
                      func = "lin",
                      cmissing = "method2")
  pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

  MassOutput <- MassWin[[1]][["Dataset"]]
  MassRand <- MassRand[[1]]
  #windowopen <- as.Date(refdateround- MassWin[[1]][["Dataset"]][[1,2]] , origin = paste0("2011-01-01"))
  #windowclose <- as.Date(refdateround - MassWin[[1]][["Dataset"]][[1,3]] , origin = paste0("2011-01-01"))
  winmedians <- medwin(MassOutput, cw = 0.95)
  medianwindowopen <- as.Date((refdateround)- (winmedians[["Median Window Open"]]) , origin = paste0("2011-01-01"))
  medianwindowclose <- as.Date((refdateround) - (winmedians[["Median Window Close"]]) , origin = paste0("2011-01-01"))

  df <- data.frame(w, round(mean(biodata$max_rate_DOY)),refdate$Month, refdate$Day,MassWin[[1]][["Dataset"]][[1,2]], MassWin[[1]][["Dataset"]][[1,3]],MassOutput[1,4],as.character(medianwindowopen), as.character(medianwindowclose))#add j #add/7
  names(dffinal) <- names(df)
  dffinal <- rbind(dffinal, df)
  png(filename = paste("SCBI", "mettower","max_rate_DOY",w,  ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300) #add w
  plotalloutput <- plotall(dataset = MassOutput,
                           datasetrand = MassRand,
                           bestmodel = MassWin[[1]]$BestModel,
                           bestmodeldata = MassWin[[1]]$BestModelData)

  dev.off()
}#}#}
dffinal <- dffinal[-1,]
names(dffinal) <- c("wood_type", "refdoy", "refmonth", "refday","winopendoy", "winclosedoy","bestmodel_beta","median_windowopendate", "median_windowclosedate")
write.csv(dffinal, file = "daily_climwin_results_max_rate_DOY.csv", row.names = FALSE)

####Max rate DOY WEEKLY ----
#rangedates <- data.frame("feb1", round(30/7)) #/7 for week
#rangedates2 <- data.frame("mar1", round(59/7)) #/7 for week
#names(rangedates) <- names(rangedates2)
#rangedates <- rbind(rangedates, rangedates2)
#colnames(rangedates) <- c("day", "doy")

dffinal <- data.frame(1,1,1,1,1,1,1,1,1)
for(w in unique(Wood_pheno_table$wood_type)){
  #for(j in unique(Wood_pheno_table$perc)){
  twentyfive <- subset(Wood_pheno_table, wood_type == w)# & perc == j )

  biodata <- data.frame(NULL)
  for(i in c(2011:2019)){ #Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
    df <- subset(twentyfive, year == i) #using twentyfive dataset
    df$date <- as.Date(df$max_rate_DOY, origin = paste0(i, "-01-01"))
    df$date <- strftime(df$date, format = "%d/%m/%Y")
    biodata <- rbind(biodata, df)
  }

  refdateround <- round(mean(biodata$max_rate_DOY)/7) #/7 for week

  #for (k in rangedates$doy) {

  refdate <- data.frame(round(mean(biodata$max_rate_DOY)))
  refdate$round.mean.biodata.max_rate_DOY.. <- as.Date(refdate$round.mean.biodata.max_rate_DOY.., origin = paste0("2011-01-01"))
  refdate <- separate(refdate, "round.mean.biodata.max_rate_DOY..", c("Year", "Month", "Day"), sep = "-")

  MassWin <- slidingwin(xvar = list(Temp = climate$TMAX),
                        cdate = climate$DATE,
                        bdate = biodata$date,
                        baseline = lm(max_rate_DOY ~ 1, data = biodata),
                        cinterval = "week",
                        range = c((refdateround-(30/7)), 0),
                        type = "absolute", refday = c(refdate$Day, refdate$Month),
                        stat = "mean",
                        func = "lin",
                        cmissing = "method2")

  MassRand <- randwin(repeats = 1,
                      xvar = list(Temp = climate$TMAX),
                      cdate = climate$DATE,
                      bdate = biodata$date,
                      baseline = lm(max_rate_DOY ~ 1, data = biodata),
                      cinterval = "week",
                      range = c((refdateround-(30/7)), 0),
                      type = "absolute", refday = c(refdate$Day,refdate$Month),
                      stat = "mean",
                      func = "lin",
                      cmissing = "method2")
  pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

  MassOutput <- MassWin[[1]][["Dataset"]]
  MassRand <- MassRand[[1]]

  #windowopen <- as.Date(refdateround- MassWin[[1]][["Dataset"]][[1,2]] , origin = paste0("2011-01-01"))
  #windowclose <- as.Date(refdateround - MassWin[[1]][["Dataset"]][[1,3]] , origin = paste0("2011-01-01"))
  winmedians <- medwin(MassOutput, cw = 0.95)
  medianwindowopen <- as.Date((refdateround*7)- (winmedians[["Median Window Open"]]*7) , origin = paste0("2011-01-01"))
  medianwindowclose <- as.Date((refdateround*7) - (winmedians[["Median Window Close"]]*7) , origin = paste0("2011-01-01"))

  df <- data.frame(w, round(mean(biodata$max_rate_DOY)/7),refdate$Month, refdate$Day, MassWin[[1]][["Dataset"]][[1,2]], MassWin[[1]][["Dataset"]][[1,3]],MassOutput[1,4],as.character(medianwindowopen), as.character(medianwindowclose))#add j #add/7
  names(dffinal) <- names(df)
  dffinal <- rbind(dffinal, df)
  png(filename = paste("SCBI", "mettower","max_rate_DOY",w,"weekly",  ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300) #add w
  plotalloutput <- plotall(dataset = MassOutput,
                           datasetrand = MassRand,
                           bestmodel = MassWin[[1]]$BestModel,
                           bestmodeldata = MassWin[[1]]$BestModelData)

  dev.off()
}#}#}
dffinal <- dffinal[-1,]
names(dffinal) <- c("wood_type", "refdoy", "refmonth", "refday","winopendoy", "winclosedoy","bestmodel_beta","median_windowopendate", "median_windowclosedate")
write.csv(dffinal, file = "weekly_climwin_results_max_rate_DOY.csv", row.names = FALSE)
plotweights(MassOutput)
