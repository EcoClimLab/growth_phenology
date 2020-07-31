library(zoo)
library(data.table)
library(readr)
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V4.csv")
twentyfive <- subset(Wood_pheno_table, perc == .25)# & sp == "litu")

#Temperature
#NEON_summary_temp <- read_csv("climate data/NEON_summary_temp.csv")
weatherdata <- read_csv("climate data/NCDC_NOAA_precip_temp.csv")
weatherdata$months <- months(as.Date(weatherdata$DATE))

#Temperatures
springweather <- subset(weatherdata, months == "January" | months == "February" | months == "March" | months == "April" | months == "May")
springweather <- springweather[complete.cases(springweather$TMAX),]
january <- subset(springweather, months == "January")
janmeans <- aggregate(january$TMAX, by = list(january$year), FUN = mean)
colnames(janmeans) <- c("year", "janmean")
february <- subset(springweather, months == "February")
febmeans <- aggregate(february$TMAX, by = list(february$year), FUN = mean)
colnames(febmeans) <- c("year", "febmean")

march <- subset(springweather, months == "March")
marchmeans <- aggregate(march$TMAX, by = list(march$year), FUN = mean)
colnames(marchmeans) <- c("year", "marchmean")

april <- subset(springweather, months == "April")
aprilmeans <- aggregate(april$TMAX, by = list(april$year), FUN = mean)
colnames(aprilmeans) <- c("year", "aprilmean")

#correlation between max rate DOY and temp means
meanratedoy <- aggregate(Wood_pheno_table$max_rate_DOY, by = list(Wood_pheno_table$year), FUN = mean)

janmeans <- janmeans[janmeans$year %in% meanratedoy$Group.1, ]
plot(meanratedoy$x~janmeans$janmean)
janlm <- lm(meanratedoy$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meanratedoy$Group.1,]
plot(meanratedoy$x~febmeans$febmean)
feblm <- lm(meanratedoy$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meanratedoy$Group.1,]
plot(meanratedoy$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 25% DOY (all trees)" )
marchlm <- lm(meanratedoy$x~marchmeans$marchmean)
cor(meanratedoy$x, marchmeans$marchmean)
abline(lm(meanratedoy$x~marchmeans$marchmean))
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meanratedoy$Group.1,]
plot(meanratedoy$x~aprilmeans$aprilmean)
aprillm <- lm(meanratedoy$x~aprilmeans$aprilmean)
summary(aprillm)


#Diffuse only
diffuse <- subset(twentyfive, wood_type == "diffuse-porous")
meanratedoydiffuse <- aggregate(diffuse$max_rate_DOY, by = list(diffuse$year), FUN = mean)

janmeans <- janmeans[janmeans$year %in% meanratedoydiffuse$Group.1, ]
plot(meanratedoydiffuse$x~janmeans$janmean)
janlm <- lm(meanratedoydiffuse$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meanratedoydiffuse$Group.1,]
plot(meanratedoydiffuse$x~febmeans$febmean)
feblm <- lm(meanratedoydiffuse$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meanratedoydiffuse$Group.1,]
plot(meanratedoydiffuse$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 25% DOY (all trees)" )
marchlm <- lm(meanratedoydiffuse$x~marchmeans$marchmean)
cor(meanratedoydiffuse$x, marchmeans$marchmean)
abline(lm(meanratedoydiffuse$x~marchmeans$marchmean))
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meanratedoydiffuse$Group.1,]
plot(meanratedoydiffuse$x~aprilmeans$aprilmean)
aprillm <- lm(meanratedoydiffuse$x~aprilmeans$aprilmean)
summary(aprillm)

#Ring porous only
ring <- subset(twentyfive, wood_type == "ring porous")
meanratedoyring <- aggregate(ring$max_rate_DOY, by = list(ring$year), FUN = mean)

janmeans <- janmeans[janmeans$year %in% meanratedoyring$Group.1, ]
plot(meanratedoyring$x~janmeans$janmean)
janlm <- lm(meanratedoyring$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meanratedoyring$Group.1,]
plot(meanratedoyring$x~febmeans$febmean)
feblm <- lm(meanratedoyring$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meanratedoyring$Group.1,]
plot(meanratedoyring$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and Mean maximum rate DOY (ring-porous)" )
marchlm <- lm(meanratedoyring$x~marchmeans$marchmean)
cor(meanratedoyring$x, marchmeans$marchmean)
abline(lm(meanratedoyring$x~marchmeans$marchmean))
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meanratedoyring$Group.1,]
plot(meanratedoyring$x~aprilmeans$aprilmean)
aprillm <- lm(meanratedoyring$x~aprilmeans$aprilmean)
summary(aprillm)


#correlation between max rate and temp means
meanrate <- aggregate(Wood_pheno_table$max_rate, by = list(Wood_pheno_table$year), FUN = mean)

janmeans <- janmeans[janmeans$year %in% meanrate$Group.1, ]
plot(meanrate$x~janmeans$janmean)
janlm <- lm(meanrate$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meanrate$Group.1,]
plot(meanrate$x~febmeans$febmean)
feblm <- lm(meanrate$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meanrate$Group.1,]
plot(meanrate$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 25% DOY (all trees)" )
marchlm <- lm(meanrate$x~marchmeans$marchmean)
cor(meanrate$x, marchmeans$marchmean)
abline(lm(meanrate$x~marchmeans$marchmean))
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meanrate$Group.1,]
plot(meanrate$x~aprilmeans$aprilmean)
aprillm <- lm(meanrate$x~aprilmeans$aprilmean)
summary(aprillm)


#Diffuse only
diffuse <- subset(twentyfive, wood_type == "diffuse-porous")
meanratediffuse <- aggregate(diffuse$max_rate, by = list(diffuse$year), FUN = mean)

janmeans <- janmeans[janmeans$year %in% meanratediffuse$Group.1, ]
plot(meanratediffuse$x~janmeans$janmean)
janlm <- lm(meanratediffuse$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meanratediffuse$Group.1,]
plot(meanratediffuse$x~febmeans$febmean)
feblm <- lm(meanratediffuse$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meanratediffuse$Group.1,]
plot(meanratediffuse$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 25% DOY (all trees)" )
marchlm <- lm(meanratediffuse$x~marchmeans$marchmean)
cor(meanratediffuse$x, marchmeans$marchmean)
abline(lm(meanratediffuse$x~marchmeans$marchmean))
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meanratediffuse$Group.1,]
plot(meanratediffuse$x~aprilmeans$aprilmean)
aprillm <- lm(meanratediffuse$x~aprilmeans$aprilmean)
summary(aprillm)

#Ring porous only
ring <- subset(twentyfive, wood_type == "ring porous")
meanratering <- aggregate(ring$max_rate, by = list(ring$year), FUN = mean)

janmeans <- janmeans[janmeans$year %in% meanratering$Group.1, ]
plot(meanratering$x~janmeans$janmean)
janlm <- lm(meanratering$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meanratering$Group.1,]
plot(meanratering$x~febmeans$febmean)
feblm <- lm(meanratering$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meanratering$Group.1,]
plot(meanratering$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 25% DOY (all trees)" )
marchlm <- lm(meanratering$x~marchmeans$marchmean)
cor(meanratering$x, marchmeans$marchmean)
abline(lm(meanratering$x~marchmeans$marchmean))
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meanratering$Group.1,]
plot(meanratering$x~aprilmeans$aprilmean)
aprillm <- lm(meanratering$x~aprilmeans$aprilmean)
summary(aprillm)


#correlation between 25% doy and monthly temp means
meandoy <- aggregate(twentyfive$DOY, by = list(twentyfive$year), FUN = mean)

janmeans <- janmeans[janmeans$year %in% meandoy$Group.1, ]
plot(meandoy$x~janmeans$janmean)
janlm <- lm(meandoy$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoy$Group.1,]
plot(meandoy$x~febmeans$febmean)
feblm <- lm(meandoy$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoy$Group.1,]
plot(meandoy$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 25% DOY (all trees)" )
marchlm <- lm(meandoy$x~marchmeans$marchmean)
cor(meandoy$x, marchmeans$marchmean)
abline(lm(meandoy$x~marchmeans$marchmean))
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoy$Group.1,]
plot(meandoy$x~aprilmeans$aprilmean)
aprillm <- lm(meandoy$x~aprilmeans$aprilmean)
summary(aprillm)

#diffuse porous only
meandoydiffuse <- aggregate(diffuse$DOY, by = list(diffuse$year), FUN = mean)
janmeans <- janmeans[janmeans$year %in% meandoydiffuse$Group.1, ]
plot(meandoydiffuse$x~janmeans$janmean)
janlm <- lm(meandoydiffuse$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~febmeans$febmean)
feblm <- lm(meandoydiffuse$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 25% DOY (diffuse-porous)" )
cor(meandoydiffuse$x,marchmeans$marchmean)
abline(lm(meandoydiffuse$x~marchmeans$marchmean))
marchlm <- lm(meandoydiffuse$x~marchmeans$marchmean)
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~aprilmeans$aprilmean)
aprillm <- lm(meandoydiffuse$x~aprilmeans$aprilmean)
summary(aprillm)

#ring porous only
meandoyring <- aggregate(ring$DOY, by = list(ring$year), FUN = mean)
janmeans <- janmeans[janmeans$year %in% meandoyring$Group.1, ]
plot(meandoyring$x~janmeans$janmean)
janlm <- lm(meandoyring$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoyring$Group.1,]
plot(meandoyring$x~febmeans$febmean)
feblm <- lm(meandoyring$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoyring$Group.1,]
plot(meandoyring$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 25% DOY (ring-porous)")
abline(lm(meandoyring$x~marchmeans$marchmean))
cor(meandoyring$x,marchmeans$marchmean)
marchlm <- lm(meandoyring$x~marchmeans$marchmean)
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoyring$Group.1,]
plot(meandoyring$x~aprilmeans$aprilmean)
aprillm <- lm(meandoyring$x~aprilmeans$aprilmean)
summary(aprillm)

#qual only
qual <- subset(ring, sp == "qual")
meandoyqual<- aggregate(qual$DOY, by = list(qual$year), FUN = mean)
janmeans <- janmeans[janmeans$year %in% meandoyqual$Group.1, ]
plot(meandoyqual$x~janmeans$janmean)
janlm <- lm(meandoyqual$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~febmeans$febmean)
feblm <- lm(meandoyqual$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 25% DOY (QUAL)" )
cor(meandoyqual$x,marchmeans$marchmean)
marchlm <- lm(meandoyqual$x~marchmeans$marchmean)
abline(lm(meandoyqual$x~marchmeans$marchmean))
summary(marchlm) #.08

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~aprilmeans$aprilmean)
aprillm <- lm(meandoyqual$x~aprilmeans$aprilmean)
summary(aprillm)

#quru only
quru <- subset(ring, sp == "quru")
meandoyquru<- aggregate(quru$DOY, by = list(quru$year), FUN = median)
janmeans <- janmeans[janmeans$year %in% meandoyquru$Group.1, ]
plot(meandoyquru$x~janmeans$janmean)
janlm <- lm(meandoyquru$x~janmeans$janmean)
cor(meandoyquru$x,janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~febmeans$febmean)
cor(meandoyquru$x, febmeans$febmean)
feblm <- lm(meandoyquru$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 25% DOY (QURU)")
cor(meandoyquru$x, marchmeans$marchmean)
abline(lm(meandoyquru$x~marchmeans$marchmean))
marchlm <- lm(meandoyquru$x~marchmeans$marchmean)
summary(marchlm) #.005

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~aprilmeans$aprilmean)
cor(meandoyquru$x, aprilmeans$aprilmean)
aprillm <- lm(meandoyquru$x~aprilmeans$aprilmean)
summary(aprillm)

#correlation between 50% doy and monthly temp means
meandoy50 <- aggregate(fifty$DOY, by = list(fifty$year), FUN = mean)

janmeans <- janmeans[janmeans$year %in% meandoy50$Group.1, ]
plot(meandoy50$x~janmeans$janmean)
janlm <- lm(meandoy50$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoy50$Group.1,]
plot(meandoy50$x~febmeans$febmean)
feblm <- lm(meandoy50$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoy50$Group.1,]
plot(meandoy50$x~marchmeans$marchmean, ylab = "" )
marchlm <- lm(meandoy50$x~marchmeans$marchmean)
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoy50$Group.1,]
plot(meandoy50$x~aprilmeans$aprilmean)
aprillm <- lm(meandoy50$x~aprilmeans$aprilmean)
summary(aprillm)

#diffuse porous only
diffuse50<- subset(fifty, wood_type == "diffuse-porous")

meandoydiffuse <- aggregate(diffuse50$DOY, by = list(diffuse50$year), FUN = mean)
janmeans <- janmeans[janmeans$year %in% meandoydiffuse$Group.1, ]
plot(meandoydiffuse$x~janmeans$janmean)
janlm <- lm(meandoydiffuse$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~febmeans$febmean)
feblm <- lm(meandoydiffuse$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~marchmeans$marchmean, ylab = "" )
marchlm <- lm(meandoydiffuse$x~marchmeans$marchmean)
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~aprilmeans$aprilmean)
aprillm <- lm(meandoydiffuse$x~aprilmeans$aprilmean)
summary(aprillm)

#ring porous only
ring50<- subset(fifty, wood_type == "ring porous")

meandoyring <- aggregate(ring50$DOY, by = list(ring50$year), FUN = mean)
janmeans <- janmeans[janmeans$year %in% meandoyring$Group.1, ]
plot(meandoyring$x~janmeans$janmean)
janlm <- lm(meandoyring$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoyring$Group.1,]
plot(meandoyring$x~febmeans$febmean)
feblm <- lm(meandoyring$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoyring$Group.1,]
plot(meandoyring$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 50% DOY (ring-porous)" )
marchlm <- lm(meandoyring$x~marchmeans$marchmean)
cor(meandoyring$x, marchmeans$marchmean)
abline(lm(meandoyring$x~marchmeans$marchmean))
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoyring$Group.1,]
plot(meandoyring$x~aprilmeans$aprilmean)
aprillm <- lm(meandoyring$x~aprilmeans$aprilmean)
summary(aprillm)

#qual only
qual <- subset(ring50, sp == "qual")
meandoyqual<- aggregate(qual$DOY, by = list(qual$year), FUN = mean)
janmeans <- janmeans[janmeans$year %in% meandoyqual$Group.1, ]
plot(meandoyqual$x~janmeans$janmean)
janlm <- lm(meandoyqual$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~febmeans$febmean)
feblm <- lm(meandoyqual$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~marchmeans$marchmean, ylab = "" )
cor(meandoyqual$x,marchmeans$marchmean)
marchlm <- lm(meandoyqual$x~marchmeans$marchmean)
summary(marchlm) #.09

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~aprilmeans$aprilmean)
aprillm <- lm(meandoyqual$x~aprilmeans$aprilmean)
summary(aprillm)

#quru only
quru <- subset(ring50, sp == "quru")
meandoyquru<- aggregate(quru$DOY, by = list(quru$year), FUN = median)
janmeans <- janmeans[janmeans$year %in% meandoyquru$Group.1, ]
plot(meandoyquru$x~janmeans$janmean)
janlm <- lm(meandoyquru$x~janmeans$janmean)
cor(meandoyquru$x,janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~febmeans$febmean)
cor(meandoyquru$x, febmeans$febmean)
feblm <- lm(meandoyquru$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~marchmeans$marchmean, ylab = "" )
cor(meandoyquru$x, marchmeans$marchmean)
marchlm <- lm(meandoyquru$x~marchmeans$marchmean)
summary(marchlm) #.06

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~aprilmeans$aprilmean)
cor(meandoyquru$x, aprilmeans$aprilmean)
aprillm <- lm(meandoyquru$x~aprilmeans$aprilmean)
summary(aprillm)

#75%
ring75<- subset(seventyfive, wood_type == "ring porous")
meandoyring <- aggregate(ring75$DOY, by = list(ring75$year), FUN = mean)

janmeans <- janmeans[janmeans$year %in% meandoyring$Group.1, ]
plot(meandoyring$x~janmeans$janmean)
janlm <- lm(meandoyring$x~janmeans$janmean)
summary(janlm)

febmeans <- febmeans[febmeans$year %in% meandoyring$Group.1,]
plot(meandoyring$x~febmeans$febmean)
feblm <- lm(meandoyring$x~febmeans$febmean)
summary(feblm)

marchmeans <- marchmeans[marchmeans$year %in% meandoyring$Group.1,]
plot(meandoyring$x~marchmeans$marchmean, ylab = "DOY", xlab = "Mean temp", main = "Correlation between March temp and 75% DOY (ring-porous)" )
marchlm <- lm(meandoyring$x~marchmeans$marchmean)
cor(meandoyring$x, marchmeans$marchmean)
abline(lm(meandoyring$x~marchmeans$marchmean))
summary(marchlm)

aprilmeans <- aprilmeans[aprilmeans$year %in% meandoyring$Group.1,]
plot(meandoyring$x~aprilmeans$aprilmean)
aprillm <- lm(meandoyring$x~aprilmeans$aprilmean)
summary(aprillm)


######################################### Precipitation ####################################
springweather <- subset(weatherdata, months == "January" | months == "February" | months == "March" | months == "April" | months == "May")
springweather <- springweather[complete.cases(springweather$PRCP),]

january <- subset(springweather, months == "January")
jansums <- aggregate(january$PRCP, by = list(january$year), FUN = sum)
colnames(jansums) <- c("year", "jansum")

february <- subset(springweather, months == "February")
febsums <- aggregate(february$PRCP, by = list(february$year), FUN = sum)
colnames(febsums) <- c("year", "febsum")


march <- subset(springweather, months == "March")
marchsums <- aggregate(march$PRCP, by = list(march$year), FUN = sum)
colnames(marchsums) <- c("year", "marchsum")

april <- subset(springweather, months == "April")
aprilsums <- aggregate(april$PRCP, by = list(april$year), FUN = sum)
colnames(aprilsums) <- c("year", "aprilsum")

#correlation between 25% doy and monthly precip totals
meandoy <- aggregate(twentyfive$DOY, by = list(twentyfive$year), FUN = mean)

jansums <- jansums[jansums$year %in% meandoy$Group.1, ]
plot(meandoy$x~jansums$jansum)
janlm <- lm(meandoy$x~jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoy$Group.1,]
plot(meandoy$x~febsums$febsum)
feblm <- lm(meandoy$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoy$Group.1,]
plot(meandoy$x~marchsums$marchsum, ylab = "DOY", xlab = "sum temp", main = "Correlation between March temp and 25% DOY (all trees)" )
marchlm <- lm(meandoy$x~marchsums$marchsum)
cor(meandoy$x, marchsums$marchsum)
abline(lm(meandoy$x~marchsums$marchsum))
summary(marchlm)

aprilsums <- aprilsums[aprilsums$year %in% meandoy$Group.1,]
plot(meandoy$x~aprilsums$aprilsum)
aprillm <- lm(meandoy$x~aprilsums$aprilsum)
summary(aprillm)

#diffuse porous only
diffuse <- subset(twentyfive, wood_type == "diffuse-porous")

meandoydiffuse <- aggregate(diffuse$DOY, by = list(diffuse$year), FUN = mean)
jansums <- jansums[jansums$year %in% meandoydiffuse$Group.1, ]
plot(meandoydiffuse$x~jansums$jansum)
janlm <- lm(meandoydiffuse$x~jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~febsums$febsum)
feblm <- lm(meandoydiffuse$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~marchsums$marchsum, ylab = "DOY", xlab = "sum temp", main = "Correlation between March temp and 25% DOY (diffuse-porous)" )
cor(meandoydiffuse$x,marchsums$marchsum)
abline(lm(meandoydiffuse$x~marchsums$marchsum))
marchlm <- lm(meandoydiffuse$x~marchsums$marchsum)
summary(marchlm)

aprilsums <- aprilsums[aprilsums$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~aprilsums$aprilsum)
aprillm <- lm(meandoydiffuse$x~aprilsums$aprilsum)
summary(aprillm)

#ring porous only
ring<- subset(twentyfive, wood_type == "ring porous")

meandoyring <- aggregate(ring$DOY, by = list(ring$year), FUN = mean)
jansums <- jansums[jansums$year %in% meandoyring$Group.1, ]
plot(meandoyring$x~jansums$jansum)
janlm <- lm(meandoyring$x~jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoyring$Group.1,]
plot(meandoyring$x~febsums$febsum)
feblm <- lm(meandoyring$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoyring$Group.1,]
plot(meandoyring$x~marchsums$marchsum, ylab = "DOY", xlab = "sum temp", main = "Correlation between March temp and 25% DOY (ring-porous)")
abline(lm(meandoyring$x~marchsums$marchsum))
cor(meandoyring$x,marchsums$marchsum)
marchlm <- lm(meandoyring$x~marchsums$marchsum)
summary(marchlm)

aprilsums <- aprilsums[aprilsums$year %in% meandoyring$Group.1,]
plot(meandoyring$x~aprilsums$aprilsum)
aprillm <- lm(meandoyring$x~aprilsums$aprilsum)
summary(aprillm)

#qual only
qual <- subset(ring, sp == "qual")
meandoyqual<- aggregate(qual$DOY, by = list(qual$year), FUN = sum)
jansums <- jansums[jansums$year %in% meandoyqual$Group.1, ]
plot(meandoyqual$x~jansums$jansum)
janlm <- lm(meandoyqual$x~jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~febsums$febsum)
feblm <- lm(meandoyqual$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~marchsums$marchsum, ylab = "DOY", xlab = "sum temp", main = "Correlation between March temp and 25% DOY (QUAL)" )
cor(meandoyqual$x,marchsums$marchsum)
marchlm <- lm(meandoyqual$x~marchsums$marchsum)
abline(lm(meandoyqual$x~marchsums$marchsum))
summary(marchlm) #.08

aprilsums <- aprilsums[aprilsums$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~aprilsums$aprilsum)
aprillm <- lm(meandoyqual$x~aprilsums$aprilsum)
summary(aprillm)

#quru only
quru <- subset(ring, sp == "quru")
meandoyquru<- aggregate(quru$DOY, by = list(quru$year), FUN = median)
jansums <- jansums[jansums$year %in% meandoyquru$Group.1, ]
plot(meandoyquru$x~jansums$jansum)
janlm <- lm(meandoyquru$x~jansums$jansum)
cor(meandoyquru$x,jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~febsums$febsum)
cor(meandoyquru$x, febsums$febsum)
feblm <- lm(meandoyquru$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~marchsums$marchsum, ylab = "DOY", xlab = "sum temp", main = "Correlation between March temp and 25% DOY (QURU)")
cor(meandoyquru$x, marchsums$marchsum)
abline(lm(meandoyquru$x~marchsums$marchsum))
marchlm <- lm(meandoyquru$x~marchsums$marchsum)
summary(marchlm) #.005

aprilsums <- aprilsums[aprilsums$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~aprilsums$aprilsum)
cor(meandoyquru$x, aprilsums$aprilsum)
aprillm <- lm(meandoyquru$x~aprilsums$aprilsum)
summary(aprillm)

#correlation between 50% doy and monthly temp sums
meandoy50 <- aggregate(fifty$DOY, by = list(fifty$year), FUN = sum)

jansums <- jansums[jansums$year %in% meandoy50$Group.1, ]
plot(meandoy50$x~jansums$jansum)
janlm <- lm(meandoy50$x~jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoy50$Group.1,]
plot(meandoy50$x~febsums$febsum)
feblm <- lm(meandoy50$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoy50$Group.1,]
plot(meandoy50$x~marchsums$marchsum, ylab = "" )
marchlm <- lm(meandoy50$x~marchsums$marchsum)
summary(marchlm)

aprilsums <- aprilsums[aprilsums$year %in% meandoy50$Group.1,]
plot(meandoy50$x~aprilsums$aprilsum)
aprillm <- lm(meandoy50$x~aprilsums$aprilsum)
summary(aprillm)

#diffuse porous only
diffuse50<- subset(fifty, wood_type == "diffuse-porous")

meandoydiffuse <- aggregate(diffuse50$DOY, by = list(diffuse50$year), FUN = sum)
jansums <- jansums[jansums$year %in% meandoydiffuse$Group.1, ]
plot(meandoydiffuse$x~jansums$jansum)
janlm <- lm(meandoydiffuse$x~jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~febsums$febsum)
feblm <- lm(meandoydiffuse$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~marchsums$marchsum, ylab = "" )
marchlm <- lm(meandoydiffuse$x~marchsums$marchsum)
summary(marchlm)

aprilsums <- aprilsums[aprilsums$year %in% meandoydiffuse$Group.1,]
plot(meandoydiffuse$x~aprilsums$aprilsum)
aprillm <- lm(meandoydiffuse$x~aprilsums$aprilsum)
summary(aprillm)

#ring porous only
ring50<- subset(fifty, wood_type == "ring porous")

meandoyring <- aggregate(ring50$DOY, by = list(ring50$year), FUN = sum)
jansums <- jansums[jansums$year %in% meandoyring$Group.1, ]
plot(meandoyring$x~jansums$jansum)
janlm <- lm(meandoyring$x~jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoyring$Group.1,]
plot(meandoyring$x~febsums$febsum)
feblm <- lm(meandoyring$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoyring$Group.1,]
plot(meandoyring$x~marchsums$marchsum, ylab = "DOY", xlab = "sum temp", main = "Correlation between March temp and 50% DOY (ring-porous)" )
marchlm <- lm(meandoyring$x~marchsums$marchsum)
cor(meandoyring$x, marchsums$marchsum)
abline(lm(meandoyring$x~marchsums$marchsum))
summary(marchlm)

aprilsums <- aprilsums[aprilsums$year %in% meandoyring$Group.1,]
plot(meandoyring$x~aprilsums$aprilsum)
aprillm <- lm(meandoyring$x~aprilsums$aprilsum)
summary(aprillm)

#qual only
qual <- subset(ring50, sp == "qual")
meandoyqual<- aggregate(qual$DOY, by = list(qual$year), FUN = sum)
jansums <- jansums[jansums$year %in% meandoyqual$Group.1, ]
plot(meandoyqual$x~jansums$jansum)
janlm <- lm(meandoyqual$x~jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~febsums$febsum)
feblm <- lm(meandoyqual$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~marchsums$marchsum, ylab = "" )
cor(meandoyqual$x,marchsums$marchsum)
marchlm <- lm(meandoyqual$x~marchsums$marchsum)
summary(marchlm) #.09

aprilsums <- aprilsums[aprilsums$year %in% meandoyqual$Group.1,]
plot(meandoyqual$x~aprilsums$aprilsum)
aprillm <- lm(meandoyqual$x~aprilsums$aprilsum)
summary(aprillm)

#quru only
quru <- subset(ring50, sp == "quru")
meandoyquru<- aggregate(quru$DOY, by = list(quru$year), FUN = median)
jansums <- jansums[jansums$year %in% meandoyquru$Group.1, ]
plot(meandoyquru$x~jansums$jansum)
janlm <- lm(meandoyquru$x~jansums$jansum)
cor(meandoyquru$x,jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~febsums$febsum)
cor(meandoyquru$x, febsums$febsum)
feblm <- lm(meandoyquru$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~marchsums$marchsum, ylab = "" )
cor(meandoyquru$x, marchsums$marchsum)
marchlm <- lm(meandoyquru$x~marchsums$marchsum)
summary(marchlm) #.06

aprilsums <- aprilsums[aprilsums$year %in% meandoyquru$Group.1,]
plot(meandoyquru$x~aprilsums$aprilsum)
cor(meandoyquru$x, aprilsums$aprilsum)
aprillm <- lm(meandoyquru$x~aprilsums$aprilsum)
summary(aprillm)

#75%
ring75<- subset(seventyfive, wood_type == "ring porous")
meandoyring <- aggregate(ring75$DOY, by = list(ring75$year), FUN = sum)

jansums <- jansums[jansums$year %in% meandoyring$Group.1, ]
plot(meandoyring$x~jansums$jansum)
janlm <- lm(meandoyring$x~jansums$jansum)
summary(janlm)

febsums <- febsums[febsums$year %in% meandoyring$Group.1,]
plot(meandoyring$x~febsums$febsum)
feblm <- lm(meandoyring$x~febsums$febsum)
summary(feblm)

marchsums <- marchsums[marchsums$year %in% meandoyring$Group.1,]
plot(meandoyring$x~marchsums$marchsum, ylab = "DOY", xlab = "sum temp", main = "Correlation between March temp and 75% DOY (ring-porous)" )
marchlm <- lm(meandoyring$x~marchsums$marchsum)
cor(meandoyring$x, marchsums$marchsum)
abline(lm(meandoyring$x~marchsums$marchsum))
summary(marchlm)

aprilsums <- aprilsums[aprilsums$year %in% meandoyring$Group.1,]
plot(meandoyring$x~aprilsums$aprilsum)
aprillm <- lm(meandoyring$x~aprilsums$aprilsum)
summary(aprillm)


############################################################################################
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
NEON_summary_precip <- read_csv("climate data/NEON_summary_precip.csv") #Work with NCDC data instead
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
