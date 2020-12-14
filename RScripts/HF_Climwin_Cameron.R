# Setup ------------------------------------------------------------------------
#
# This script generates Harvard Forest climwin critical temperature windows 6 different
# types of outcome variables y and outputs them to csv
#
# 1. wood phenology DOY25, DOY50, DOY75 data
# 2. raw fitted parameters and related values (max rate, max rate DOY, etc.)
#
# which we will then "clean" for outliers in data_cleaning.R.
#
# To get a quick overview of the sections of this code, go to RStudio menu bar ->
# Edit -> Folding -> Collapse all.
#
library(climwin)
library(readr)
library(lubridate)
library(tidyverse)
library(dplyr)

#Edit the plotbetas function in climwin
#In the function, change scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") to scale_fill_gradient2(high = "blue", mid = "yellow", low = "red")
trace("plotbetas", edit = TRUE)

#ggplot(MassOutput, aes(x = WindowClose, y = WindowOpen, z = ModelBeta)) +
#  geom_tile(aes(fill = ModelBeta)) +
#  scale_fill_gradient2(high = "blue", mid = "yellow", low = "red") +
#  #theme_climwin() +
#  theme(legend.position = c(0.75,0.3)) +
#  ggtitle("Beta linear") +
#  ylab("Window open") +
#  xlab("Window close")


# Merge two weather DF's into one ----
#HF_1964to2002 <- read_csv("climate data/HF_1964to2002.csv")
#HF_2001toPresent <- read_csv("climate data/HF_2001toPresent.csv")

#HF_1964to2002$DOY <- yday(HF_1964to2002$date) # format = "%y-%m-%d")
#
#HF_1964to2002$year <- as.numeric(format(HF_1964to2002$date, format = "%Y"))
#HF_1998to2002 <- subset(HF_1964to2002, year >= 1998)
#HF_1998to2002 <- HF_1998to2002[, c(1, 6, 10, 11)]
#HF_1998to2002$month <- as.numeric(format(HF_1998to2002$date, format = "%m"))
#HF_1998to2002$day <- as.numeric(format(HF_1998to2002$date, format = "%d"))#
#
#HF_2001toPresent$DOY <- yday(HF_2001toPresent$date)
#HF_2001toPresent$year <- as.numeric(format(HF_2001toPresent$date, format = "%Y"))
#HF_2001toPresent <- HF_2001toPresent[, c(1, 7, 47, 48)]
#HF_2001toPresent <- HF_2001toPresent[c(-1:-505), ]
#HF_2001toPresent <- subset(HF_2001toPresent, year == 2002 | year == 2003)
#HF_2001toPresent$month <- as.numeric(format(HF_2001toPresent$date, format = "%m"))
#HF_2001toPresent$day <- as.numeric(format(HF_2001toPresent$date, format = "%d"))
#
#HF_weatherdata <- rbind(HF_1998to2002, HF_2001toPresent)
#
#write.csv(HF_weatherdata, file = "HF_weatherdata_TMIN.csv", row.names = FALSE)
# Read in climate and biological data for climwin analysis ----
climate <- read_csv("climate data/HF_weatherdata.csv", col_names = TRUE)

colnames(climate) <- c("date", "TMAX", "doy", "year", "month", "day")

climate <- climate[complete.cases(climate$TMAX), ]

climate$DATE <- paste(climate$day, climate$month, climate$year, sep = "/")
climate$DATE <- strptime(as.character(climate$DATE), format = "%d/%m/%Y")
climate$DATE <- format(climate$DATE, "%d/%m/%Y")

# The data containing the biological responses for testing
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_HarvardForest_CLEAN.csv") # Master datafrmae containing 20%, 50%, and 75% growth milestones
## TMAX: Percentage DOY climwin all wood types, all percs WEEKLY ----
dffinal <- data.frame(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
for (w in unique(Wood_pheno_table$wood_type)) {
  for (j in unique(Wood_pheno_table$perc)) {
    twentyfive <- subset(Wood_pheno_table, wood_type == w & perc == j) #

    biodata <- data.frame(NULL)
    for (i in c(1998:2003)) { # Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
      df <- subset(twentyfive, year == i) # using twentyfive dataset
      df$date <- as.Date(df$DOY, origin = paste0(i, "-01-01"))
      df$date <- strftime(df$date, format = "%d/%m/%Y")
      biodata <- rbind(biodata, df)
    }

    refdateround <- round(mean(biodata$DOY) / 7) # /7 for week

    # for (k in rangedates$doy) {

    refdate <- data.frame(round(mean(biodata$DOY)))
    refdate$round.mean.biodata.DOY.. <- as.Date(refdate$round.mean.biodata.DOY.., origin = paste0("1998-01-01"))
    refdate <- separate(refdate, "round.mean.biodata.DOY..", c("Year", "Month", "Day"), sep = "-")

    MassWin <- slidingwin(
      xvar = list(Temp = climate$TMAX),
      cdate = climate$DATE,
      bdate = biodata$date,
      baseline = lm(DOY ~ 1, data = biodata),
      cinterval = "week",
      range = c((refdateround - (30 / 7)), 0),
      type = "absolute", refday = c(refdate$Day, refdate$Month),
      stat = "mean",
      func = "lin",
      cmissing = "method2"
    )

    MassRand <- randwin(
      repeats = 500,
      xvar = list(Temp = climate$TMAX),
      cdate = climate$DATE,
      bdate = biodata$date,
      baseline = lm(DOY ~ 1, data = biodata),
      cinterval = "week",
      range = c((refdateround - (30 / 7)), 0),
      type = "absolute", refday = c(refdate$Day, refdate$Month),
      stat = "mean",
      func = "lin",
      cmissing = "method2"
    )
    # pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

    MassOutput <- MassWin[[1]][["Dataset"]]
    write.csv(MassOutput, file = paste0("MassOutput_", j,"_", w, ".csv"), row.names = FALSE)
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

    png(filename = paste("doc/manuscript/tables_figures/","HF", w, j, ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300) # add w
    plotalloutput <- plotall(
      dataset = MassOutput,
      datasetrand = MassRand,
      bestmodel = MassWin[[1]]$BestModel,
      bestmodeldata = MassWin[[1]]$BestModelData,
      arrow = TRUE
    )
    dev.off()


    plotbetas(MassOutput, arrow = TRUE)
    ggsave(filename = paste("doc/manuscript/tables_figures/","HF","Plotbetas", w, j, ".png", sep = "_"), width = 10, height = 8, units = "in") # add w


  }
} # }

dffinal <- dffinal[-1, ]
names(dffinal) <- c("wood_type", "percs", "refwoy", "refmonth", "refday", "winopenwoy", "winclosewoy", "bestmodel_beta", "median_windowopendate", "median_windowclosedate")
write.csv(dffinal, file = "results/Climwin_results/Weekly/Harvard Forest/weekly_climwin_results_HF_TMAX.csv", row.names = FALSE)


## TMIN: Percentage DOY climwin all wood types, all percs WEEKLY ----
climate <- read_csv("climate data/HF_weatherdata_TMIN.csv", col_names = TRUE)

colnames(climate) <- c("date", "TMIN", "doy", "year", "month", "day")

climate <- climate[complete.cases(climate$TMIN), ]

climate$DATE <- paste(climate$day, climate$month, climate$year, sep = "/")
climate$DATE <- strptime(as.character(climate$DATE), format = "%d/%m/%Y")
climate$DATE <- format(climate$DATE, "%d/%m/%Y")

dffinal <- data.frame(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
for (w in unique(Wood_pheno_table$wood_type)) {
  for (j in unique(Wood_pheno_table$perc)) {
    twentyfive <- subset(Wood_pheno_table, wood_type == w & perc == j) #

    biodata <- data.frame(NULL)
    for (i in c(1998:2003)) { # Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
      df <- subset(twentyfive, year == i) # using twentyfive dataset
      df$date <- as.Date(df$DOY, origin = paste0(i, "-01-01"))
      df$date <- strftime(df$date, format = "%d/%m/%Y")
      biodata <- rbind(biodata, df)
    }

    refdateround <- round(mean(biodata$DOY) / 7) # /7 for week


    refdate <- data.frame(round(mean(biodata$DOY)))
    refdate$round.mean.biodata.DOY.. <- as.Date(refdate$round.mean.biodata.DOY.., origin = paste0("1998-01-01"))
    refdate <- separate(refdate, "round.mean.biodata.DOY..", c("Year", "Month", "Day"), sep = "-")

    MassWin <- slidingwin(
      xvar = list(Temp = climate$TMIN),
      cdate = climate$DATE,
      bdate = biodata$date,
      baseline = lm(DOY ~ 1, data = biodata),
      cinterval = "week",
      range = c((refdateround - (30 / 7)), 0),
      type = "absolute", refday = c(refdate$Day, refdate$Month),
      stat = "mean",
      func = "lin",
      cmissing = "method2"
    )

    MassRand <- randwin(
      repeats = 50,
      xvar = list(Temp = climate$TMIN),
      cdate = climate$DATE,
      bdate = biodata$date,
      baseline = lm(DOY ~ 1, data = biodata),
      cinterval = "week",
      range = c((refdateround - (30 / 7)), 0),
      type = "absolute", refday = c(refdate$Day, refdate$Month),
      stat = "mean",
      func = "lin",
      cmissing = "method2"
    )
    # pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

    MassOutput <- MassWin[[1]][["Dataset"]]
    write.csv(MassOutput, file = paste0("Results/Climwin_results/Weekly/Harvard Forest/TMIN/", "MassOutput_",j, w, ".csv"), row.names = FALSE)
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
    png(filename = paste("doc/manuscript/tables_figures/","HF", w, j, ".png", sep = "_"), width = 10, height = 8, units = "in", res = 300) # add w
    plotalloutput <- plotall(
      dataset = MassOutput,
      datasetrand = MassRand,
      bestmodel = MassWin[[1]]$BestModel,
      bestmodeldata = MassWin[[1]]$BestModelData,
      arrow = TRUE
    )
    dev.off()
    plotbetas(MassOutput, arrow = TRUE)
    ggsave(filename = paste("doc/manuscript/tables_figures/","HF","Plotbetas", w, j, ".png", sep = "_"), width = 10, height = 8, units = "in") # add w


  }
} # }
dffinal <- dffinal[-1, ]
names(dffinal) <- c("wood_type", "percs", "refwoy", "refmonth", "refday", "winopenwoy", "winclosewoy", "bestmodel_beta", "median_windowopendate", "median_windowclosedate")
write.csv(dffinal, file = "results/Climwin_results/Weekly/Harvard Forest/TMIN/weekly_climwin_results_HF_TMIN.csv", row.names = FALSE)


