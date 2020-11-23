# Setup ------------------------------------------------------------------------
#
# This script generates SCBI climwin critical temperature windows 6 different
# types of outcome variables y and outputs them to csv
#
# To get a quick overview of the sections of this code, go to RStudio menu bar ->
# Edit -> Folding -> Collapse all.
#
library(climwin)
library(readr)
library(lubridate)
library(tidyverse)

# Read in climate and biological data for climwin analysis ----
# SCBI met tower
climate <- read_csv("climate data/met_tower_data_sensor2_ncdc_supplemented.csv", col_names = TRUE) # only goes to October 2019, fix?

colnames(climate) <- c("date", "year", "month", "day", "doy", "TMAX")
climate[climate$TMAX == -99.9, ] <- NA

climate <- climate[complete.cases(climate$TMAX), ]

climate$DATE <- paste(climate$day, climate$month, climate$year, sep = "/")
climate$DATE <- strptime(as.character(climate$DATE), format = "%d/%m/%Y")
climate$DATE <- format(climate$DATE, "%d/%m/%Y")
climate <- distinct(climate, DATE, .keep_all = TRUE)
# The data containing the biological responses for testing
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_SCBI_CLEAN.csv") # Master datafrmae containing 20%, 50%, and 75% growth milestones

## Percentage DOY climwin all wood types, all percs WEEKLY ----
## TMAX: Percentage DOY climwin all wood types, all percs WEEKLY ----

dffinal <- data.frame(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
for (w in unique(Wood_pheno_table$wood_type)) {
  for (j in unique(Wood_pheno_table$perc)) {
    twentyfive <- subset(Wood_pheno_table, wood_type == w & perc == j) #

    biodata <- data.frame(NULL)
    for (i in c(2011:2020)) { # Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
      df <- subset(twentyfive, year == i) # using twentyfive dataset
      df$date <- as.Date(df$DOY, origin = paste0(i, "-01-01"))
      df$date <- strftime(df$date, format = "%d/%m/%Y")
      biodata <- rbind(biodata, df)
    }

    refdateround <- round(mean(biodata$DOY) / 7) # /7 for week

    # for (k in rangedates$doy) {

    refdate <- data.frame(round(mean(biodata$DOY)))
    refdate$round.mean.biodata.DOY.. <- as.Date(refdate$round.mean.biodata.DOY.., origin = paste0("2011-01-01"))
    refdate <- separate(refdate, "round.mean.biodata.DOY..", c("Year", "Month", "Day"), sep = "-")

    MassWin <- slidingwin(
      xvar = list(Temp = climate$TMAX),
      cdate = climate$DATE,
      bdate = biodata$date,
      baseline = lm(DOY ~ 1, data = biodata),
      cinterval = "week",
      range = c(refdateround, 0),
      type = "absolute", refday = c(refdate$Day, refdate$Month),
      stat = "mean",
      func = "lin",
      cmissing = "method2"
    )

    MassRand <- randwin(
      repeats = 5,
      xvar = list(Temp = climate$TMAX),
      cdate = climate$DATE,
      bdate = biodata$date,
      baseline = lm(DOY ~ 1, data = biodata),
      cinterval = "week",
      range = c(refdateround, 0),
      type = "absolute", refday = c(refdate$Day, refdate$Month),
      stat = "mean",
      func = "lin",
      cmissing = "method2"
    )
    pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

    MassOutput <- MassWin[[1]][["Dataset"]]
    MassRand <- MassRand[[1]]
    # windowopen <- as.Date((refdateround*7)- (MassWin[[1]][["Dataset"]][[1,2]]*7) , origin = paste0("2011-01-01"))
    # windowclose <- as.Date((refdateround*7) - (MassWin[[1]][["Dataset"]][[1,3]]*7) , origin = paste0("2011-01-01"))
    tryCatch(
      winmedians <- medwin(MassOutput, cw = 0.95),
      error = function(hi) {
        winmedians <<- medwin(MassOutput, cw = .99999)
      }
    )
    medianwindowopen <- as.Date(refdateround * 7 - winmedians[["Median Window Open"]] * 7, origin = paste0("2011-01-01"))
    medianwindowclose <- as.Date(refdateround * 7 - winmedians[["Median Window Close"]] * 7, origin = paste0("2011-01-01"))

    df <- data.frame(w, j, round(mean(biodata$DOY) / 7), refdate$Month, refdate$Day, MassWin[[1]][["Dataset"]][[1, 2]], MassWin[[1]][["Dataset"]][[1, 3]], MassOutput[1, 4], as.character(medianwindowopen), as.character(medianwindowclose)) # add w, #add/7
    names(dffinal) <- names(df)
    dffinal <- rbind(dffinal, df)
    png(filename = paste("SCBI", "mettower", w, j, ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300) # add w
    plotalloutput <- plotall(
      dataset = MassOutput,
      datasetrand = MassRand,
      bestmodel = MassWin[[1]]$BestModel,
      bestmodeldata = MassWin[[1]]$BestModelData
    )

    dev.off()
  }
} # }
dffinal <- dffinal[-1, ]
names(dffinal) <- c("wood_type", "percs", "refwoy", "refmonth", "refday", "winopenwoy", "winclosewoy", "bestmodel_beta", "median_windowopendate", "median_windowclosedate")
write.csv(dffinal, file = "results/Climwin_results/Weekly/SCBI/weekly_climwin_results_SCBI_TMAX.csv", row.names = FALSE)



## TMIN: Percentage DOY climwin all wood types, all percs WEEKLY ----

#Set up TMIN dataframe
#climate <- read_csv("climate data/SCBI_mettower_data_sensor2.csv", col_names = FALSE)
#climate <- climate[,c(1,2,3,6)]
#names(climate) <- c("year", "month", "day", "TMIN")
#climate[climate$TMIN == -99.9,] <- NA
#climate$TMIN <- as.numeric(climate$TMIN)

#climate <- climate[complete.cases(climate$TMIN),]

#climate$DATE <- paste(climate$day, climate$month, climate$year, sep = "/")
#climate$DATE <- strptime(as.character(climate$DATE), format = "%d/%m/%Y")
#climate$doy <- yday(climate$DATE)
#climate$DATE <- format(climate$DATE, "%d/%m/%Y")
#climate <- distinct(climate, DATE, .keep_all = TRUE)

#write.csv(climate, file = "SCBI_tmin.csv", row.names = FALSE)


climate <- read_csv("climate data/SCBI_tmin.csv")

dffinal <- data.frame(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
for (w in unique(Wood_pheno_table$wood_type)) {
  for (j in unique(Wood_pheno_table$perc)) {
    twentyfive <- subset(Wood_pheno_table, wood_type == w & perc == j) #

    biodata <- data.frame(NULL)
    for (i in c(2011:2020)) { # Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
      df <- subset(twentyfive, year == i) # using twentyfive dataset
      df$date <- as.Date(df$DOY, origin = paste0(i, "-01-01"))
      df$date <- strftime(df$date, format = "%d/%m/%Y")
      biodata <- rbind(biodata, df)
    }

    refdateround <- round(mean(biodata$DOY) / 7) # /7 for week

    # for (k in rangedates$doy) {

    refdate <- data.frame(round(mean(biodata$DOY)))
    refdate$round.mean.biodata.DOY.. <- as.Date(refdate$round.mean.biodata.DOY.., origin = paste0("2011-01-01"))
    refdate <- separate(refdate, "round.mean.biodata.DOY..", c("Year", "Month", "Day"), sep = "-")

    MassWin <- slidingwin(
      xvar = list(Temp = climate$TMIN),
      cdate = climate$DATE,
      bdate = biodata$date,
      baseline = lm(DOY ~ 1, data = biodata),
      cinterval = "week",
      range = c(refdateround, 0),
      type = "absolute", refday = c(refdate$Day, refdate$Month),
      stat = "mean",
      func = "lin",
      cmissing = "method2"
    )

    MassRand <- randwin(
      repeats = 5,
      xvar = list(Temp = climate$TMIN),
      cdate = climate$DATE,
      bdate = biodata$date,
      baseline = lm(DOY ~ 1, data = biodata),
      cinterval = "week",
      range = c(refdateround, 0),
      type = "absolute", refday = c(refdate$Day, refdate$Month),
      stat = "mean",
      func = "lin",
      cmissing = "method2"
    )
    pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

    MassOutput <- MassWin[[1]][["Dataset"]]
    MassRand <- MassRand[[1]]
    # windowopen <- as.Date((refdateround*7)- (MassWin[[1]][["Dataset"]][[1,2]]*7) , origin = paste0("2011-01-01"))
    # windowclose <- as.Date((refdateround*7) - (MassWin[[1]][["Dataset"]][[1,3]]*7) , origin = paste0("2011-01-01"))
    tryCatch(
      winmedians <- medwin(MassOutput, cw = 0.95),
      error = function(hi) {
        winmedians <<- medwin(MassOutput, cw = .99999)
      }
    )
    medianwindowopen <- as.Date(refdateround * 7 - winmedians[["Median Window Open"]] * 7, origin = paste0("2011-01-01"))
    medianwindowclose <- as.Date(refdateround * 7 - winmedians[["Median Window Close"]] * 7, origin = paste0("2011-01-01"))

    df <- data.frame(w, j, round(mean(biodata$DOY) / 7), refdate$Month, refdate$Day, MassWin[[1]][["Dataset"]][[1, 2]], MassWin[[1]][["Dataset"]][[1, 3]], MassOutput[1, 4], as.character(medianwindowopen), as.character(medianwindowclose)) # add w, #add/7
    names(dffinal) <- names(df)
    dffinal <- rbind(dffinal, df)
    png(filename = paste("SCBI", "mettower", w, j, ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300) # add w
    plotalloutput <- plotall(
      dataset = MassOutput,
      datasetrand = MassRand,
      bestmodel = MassWin[[1]]$BestModel,
      bestmodeldata = MassWin[[1]]$BestModelData
    )

    dev.off()
  }
} # }
dffinal <- dffinal[-1, ]
names(dffinal) <- c("wood_type", "percs", "refwoy", "refmonth", "refday", "winopenwoy", "winclosewoy", "bestmodel_beta", "median_windowopendate", "median_windowclosedate")
write.csv(dffinal, file = "results/Climwin_results/Weekly/SCBI/weekly_climwin_results_SCBI_TMIN.csv", row.names = FALSE)
