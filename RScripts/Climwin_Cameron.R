#Climwin attempt
library(climwin)
library(readr)
#NCDC_NOAA Front Royal station data
climate <- read_csv("climate data/NCDC_NOAA_precip_temp.csv")
climate <- climate[complete.cases(climate$TMAX),]
climate <- climate[complete.cases(climate$DATE),]

#The data containing the biological responses for testing
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V4.csv") #Master datafrmae containing 20%, 50%, and 75% growth milestones
twentyfive <- subset(Wood_pheno_table, perc == .25 & wood_type == "ring porous") # subset to only 25%

#We would want to test all three percentages (25, 50, 75) but I dont know how to.
#I would also like to test how $Max_rate, $Max_rate_DOY, and $tot (total growth) are correlated with climate
biodata <- data.frame(NULL)
for(i in c(2011:2019)){ #Assigns dates in the proper format for Climwin analysis, using DOY (already in dataframe)
  df <- subset(twentyfive, year == i) #using twentyfive dataset
  df$date <- as.Date(df$DOY, origin = paste0(i, "-01-01"))
  df$date <- strftime(df$date, format = "%d/%m/%Y")
  biodata <- rbind(biodata, df)
}

round(mean(biodata$DOY))


MassWin <- slidingwin(xvar = list(Temp = climate$TMAX),
                      cdate = climate$DATE,
                      bdate = biodata$date,
                      baseline = lm(DOY ~ 1, data = biodata),
                      cinterval = "day",
                      range = c(113, 0),
                      type = "relative",
                      stat = "mean",
                      func = "lin",
                      cmissing = "method2")


MassRand <- randwin(repeats = 5,
                    xvar = list(Temp = climate$TMAX),
                    cdate = climate$DATE,
                    bdate = biodata$date,
                    baseline = lm(DOY ~ 1, data = biodata),
                    cinterval = "day",
                    range = c(113, 0), #make range the reference day to January 1st
                    type = "relative", #make reference day the mean DOY
                    stat = "mean",
                    func = "lin",
                    cmissing = "method2")

pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], metric = "AIC")

MassSingle <- singlewin(xvar = list(Temp = climate$TMAX),
                        cdate = climate$DATE,
                        bdate = biodata$date,
                        baseline = lm(DOY ~ 1, data = biodata),
                        cinterval = "day",
                        range = c(36, 0),
                        type = "relative",
                        stat = "mean",
                        func = "lin",
                        cmissing = "method2")
MassOutput <- MassWin[[1]][["Dataset"]]
plotdelta(dataset = MassOutput)
plotweights(dataset = MassOutput)
plotbetas(dataset = MassOutput)
plotwin(dataset = MassOutput)
plotbest(dataset = MassOutput,
         bestmodel = MassSingle$BestModel,
         bestmodeldata = MassSingle$BestModelData)

plotall(dataset = MassOutput,
        datasetrand = MassRand,
        bestmodel = MassSingle$BestModel,
        bestmodeldata = MassSingle$BestModelData)
