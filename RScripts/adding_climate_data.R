library(zoo)
library(data.table)

Wood_pheno_table <- read_csv("C:/Users/world/Desktop/Github/growth_phenology/Data/Wood_pheno_table_V2.csv")

#Temperature
NEON_summary_temp <- read_csv("climate data/NEON_summary_temp.csv")

NEON_summary_temp$date <- as.Date(NEON_summary_temp$date)
NEON_summary_temp$DOY <- yday(NEON_summary_temp$date)
temps <- NEON_summary_temp[,c(6, 7,8,14, 15)]

january <- seq(1,31,1)
feb <- seq(32,60, 1)
march <- seq(60, 91,1)
april <- seq(92,122,1)

jantemps <- temps[temps$DOY %in% january,]
jantemps <- jantemps[complete.cases(jantemps$wssTempTripleMean),]
janmeans <- aggregate(jantemps$wssTempTripleMean, by = list(jantemps$year), FUN = mean)
janmaxes <-  aggregate(jantemps$wssTempTripleMaximum, by = list(jantemps$year), FUN = mean)
janmins <-  aggregate(jantemps$wssTempTripleMinimum, by = list(jantemps$year), FUN = mean)


febtemps <- temps[temps$DOY %in% feb,]
febtemps <- febtemps[complete.cases(febtemps$wssTempTripleMean),]
febmeans <- aggregate(febtemps$wssTempTripleMean, by = list(febtemps$year), FUN = mean)
febmaxes <- aggregate(febtemps$wssTempTripleMaximum, by = list(febtemps$year), FUN = mean)
febmins <- aggregate(febtemps$wssTempTripleMinimum, by = list(febtemps$year), FUN = mean)


marchtemps <- temps[temps$DOY %in% march,]
marchtemps <- marchtemps[complete.cases(marchtemps$wssTempTripleMean),]
marchmeans <- aggregate(marchtemps$wssTempTripleMean, by = list(marchtemps$year), FUN = mean)
marchmaxes <- aggregate(marchtemps$wssTempTripleMaximum, by = list(marchtemps$year), FUN = mean)
marchmins <- aggregate(marchtemps$wssTempTripleMinimum, by = list(marchtemps$year), FUN = mean)

apriltemps <- temps[temps$DOY %in% april,]
apriltemps <- apriltemps[complete.cases(apriltemps$wssTempTripleMean),]
aprilmeans <- aggregate(apriltemps$wssTempTripleMean, by = list(apriltemps$year), FUN = mean)
aprilmaxes <- aggregate(apriltemps$wssTempTripleMaximum, by = list(apriltemps$year), FUN = mean)
aprilmins <- aggregate(apriltemps$wssTempTripleMinimum, by = list(apriltemps$year), FUN = mean)

Wood_pheno_table$janavg <- ifelse(Wood_pheno_table$year == 2015, janmeans[1,2],
                                  ifelse(Wood_pheno_table$year == 2016, janmeans[2,2],
                                         ifelse(Wood_pheno_table$year == 2017, janmeans[3,2],
                                                ifelse(Wood_pheno_table$year == 2018, janmeans[4,2],
                                                       ifelse(Wood_pheno_table$year == 2019, janmeans[5,2], NA)))))

Wood_pheno_table$febavg <- ifelse(Wood_pheno_table$year == 2015, febmeans[1,2],
                                  ifelse(Wood_pheno_table$year == 2016, febmeans[2,2],
                                         ifelse(Wood_pheno_table$year == 2017, febmeans[3,2],
                                                ifelse(Wood_pheno_table$year == 2018, febmeans[4,2],
                                                       ifelse(Wood_pheno_table$year == 2019, febmeans[5,2], NA)))))

Wood_pheno_table$marchavg <- ifelse(Wood_pheno_table$year == 2015, marchmeans[1,2],
                                  ifelse(Wood_pheno_table$year == 2016, marchmeans[2,2],
                                         ifelse(Wood_pheno_table$year == 2017, marchmeans[3,2],
                                                ifelse(Wood_pheno_table$year == 2018, marchmeans[4,2],
                                                       ifelse(Wood_pheno_table$year == 2019, marchmeans[5,2], NA)))))

Wood_pheno_table$aprilavg <- ifelse(Wood_pheno_table$year == 2015, aprilmeans[1,2],
                                  ifelse(Wood_pheno_table$year == 2016, aprilmeans[2,2],
                                         ifelse(Wood_pheno_table$year == 2017, aprilmeans[3,2],
                                                ifelse(Wood_pheno_table$year == 2018, aprilmeans[4,2],
                                                       ifelse(Wood_pheno_table$year == 2019, aprilmeans[5,2], NA)))))

Wood_pheno_table$janmin <- ifelse(Wood_pheno_table$year == 2015, janmins[1,2],
                                  ifelse(Wood_pheno_table$year == 2016, janmins[2,2],
                                         ifelse(Wood_pheno_table$year == 2017, janmins[3,2],
                                                ifelse(Wood_pheno_table$year == 2018, janmins[4,2],
                                                       ifelse(Wood_pheno_table$year == 2019, janmins[5,2], NA)))))

Wood_pheno_table$febmin <- ifelse(Wood_pheno_table$year == 2015, febmins[1,2],
                                  ifelse(Wood_pheno_table$year == 2016, febmins[2,2],
                                         ifelse(Wood_pheno_table$year == 2017, febmins[3,2],
                                                ifelse(Wood_pheno_table$year == 2018, febmins[4,2],
                                                       ifelse(Wood_pheno_table$year == 2019, febmins[5,2], NA)))))

Wood_pheno_table$marchmin <- ifelse(Wood_pheno_table$year == 2015, marchmins[1,2],
                                    ifelse(Wood_pheno_table$year == 2016, marchmins[2,2],
                                           ifelse(Wood_pheno_table$year == 2017, marchmins[3,2],
                                                  ifelse(Wood_pheno_table$year == 2018, marchmins[4,2],
                                                         ifelse(Wood_pheno_table$year == 2019, marchmins[5,2], NA)))))

Wood_pheno_table$aprilmin <- ifelse(Wood_pheno_table$year == 2015, aprilmins[1,2],
                                    ifelse(Wood_pheno_table$year == 2016, aprilmins[2,2],
                                           ifelse(Wood_pheno_table$year == 2017, aprilmins[3,2],
                                                  ifelse(Wood_pheno_table$year == 2018, aprilmins[4,2],
                                                         ifelse(Wood_pheno_table$year == 2019, aprilmins[5,2], NA)))))

Wood_pheno_table$janmax <- ifelse(Wood_pheno_table$year == 2015, janmaxes[1,2],
                                  ifelse(Wood_pheno_table$year == 2016, janmaxes[2,2],
                                         ifelse(Wood_pheno_table$year == 2017, janmaxes[3,2],
                                                ifelse(Wood_pheno_table$year == 2018, janmaxes[4,2],
                                                       ifelse(Wood_pheno_table$year == 2019, janmaxes[5,2], NA)))))

Wood_pheno_table$febmax <- ifelse(Wood_pheno_table$year == 2015, febmaxes[1,2],
                                  ifelse(Wood_pheno_table$year == 2016, febmaxes[2,2],
                                         ifelse(Wood_pheno_table$year == 2017, febmaxes[3,2],
                                                ifelse(Wood_pheno_table$year == 2018, febmaxes[4,2],
                                                       ifelse(Wood_pheno_table$year == 2019, febmaxes[5,2], NA)))))

Wood_pheno_table$marchmax <- ifelse(Wood_pheno_table$year == 2015, marchmaxes[1,2],
                                    ifelse(Wood_pheno_table$year == 2016, marchmaxes[2,2],
                                           ifelse(Wood_pheno_table$year == 2017, marchmaxes[3,2],
                                                  ifelse(Wood_pheno_table$year == 2018, marchmaxes[4,2],
                                                         ifelse(Wood_pheno_table$year == 2019, marchmaxes[5,2], NA)))))

Wood_pheno_table$aprilmax <- ifelse(Wood_pheno_table$year == 2015, aprilmaxes[1,2],
                                    ifelse(Wood_pheno_table$year == 2016, aprilmaxes[2,2],
                                           ifelse(Wood_pheno_table$year == 2017, aprilmaxes[3,2],
                                                  ifelse(Wood_pheno_table$year == 2018, aprilmaxes[4,2],
                                                         ifelse(Wood_pheno_table$year == 2019, aprilmaxes[5,2], NA)))))

write.csv(Wood_pheno_table, file = "wood_pheno_with_temp.csv", row.names = FALSE)

#Precipitation
NEON_summary_precip <- read_csv("C:/Users/world/Desktop/Github/growth_phenology/climate data/NEON_summary_precip.csv")
NEON_summary_precip$date <- as.Date(NEON_summary_precip$date)
NEON_summary_precip$DOY <- yday(NEON_summary_precip$date)

precip <- NEON_summary_precip[,c(6,10,11)]

janprecip <- precip[precip$DOY %in% january,]
janprecip <- janprecip[complete.cases(janprecip$wssPrecipTotal),]
janmeans <- aggregate(janprecip$wssPrecipTotal, by = list(janprecip$year), FUN = sum)


febprecip <- precip[precip$DOY %in% feb,]
febprecip <- febprecip[complete.cases(febprecip$wssPrecipTotal),]
febmeans <- aggregate(febprecip$wssPrecipTotal, by = list(febprecip$year), FUN = sum)


marchprecip <- precip[precip$DOY %in% march,]
marchprecip <- marchprecip[complete.cases(marchprecip$wssPrecipTotal),]
marchmeans <- aggregate(marchprecip$wssPrecipTotal, by = list(marchprecip$year), FUN = sum)

aprilprecip <- precip[precip$DOY %in% april,]
aprilprecip <- aprilprecip[complete.cases(aprilprecip$wssPrecipTotal),]
aprilmeans <- aggregate(aprilprecip$wssPrecipTotal, by = list(aprilprecip$year), FUN = sum)

Wood_pheno_table$janprecip <- ifelse(Wood_pheno_table$year == 2015, janmeans[1,2],
                                  ifelse(Wood_pheno_table$year == 2016, janmeans[2,2],
                                         ifelse(Wood_pheno_table$year == 2017, janmeans[3,2],
                                                ifelse(Wood_pheno_table$year == 2018, janmeans[4,2],
                                                       ifelse(Wood_pheno_table$year == 2019, janmeans[5,2], NA)))))

Wood_pheno_table$febprecip<- ifelse(Wood_pheno_table$year == 2015, febmeans[1,2],
                                  ifelse(Wood_pheno_table$year == 2016, febmeans[2,2],
                                         ifelse(Wood_pheno_table$year == 2017, febmeans[3,2],
                                                ifelse(Wood_pheno_table$year == 2018, febmeans[4,2],
                                                       ifelse(Wood_pheno_table$year == 2019, febmeans[5,2], NA)))))

Wood_pheno_table$marchprecip <- ifelse(Wood_pheno_table$year == 2015, marchmeans[1,2],
                                    ifelse(Wood_pheno_table$year == 2016, marchmeans[2,2],
                                           ifelse(Wood_pheno_table$year == 2017, marchmeans[3,2],
                                                  ifelse(Wood_pheno_table$year == 2018, marchmeans[4,2],
                                                         ifelse(Wood_pheno_table$year == 2019, marchmeans[5,2], NA)))))

Wood_pheno_table$aprilprecip <- ifelse(Wood_pheno_table$year == 2015, aprilmeans[1,2],
                                    ifelse(Wood_pheno_table$year == 2016, aprilmeans[2,2],
                                           ifelse(Wood_pheno_table$year == 2017, aprilmeans[3,2],
                                                  ifelse(Wood_pheno_table$year == 2018, aprilmeans[4,2],
                                                         ifelse(Wood_pheno_table$year == 2019, aprilmeans[5,2], NA)))))

plot(Wood_pheno_table$marchmax ~ Wood_pheno_table)
