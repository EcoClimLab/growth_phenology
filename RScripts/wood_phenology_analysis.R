library(readr)
Wood_pheno_table <- read_csv("C:/Users/world/Desktop/Github/growth_phenology/Data/Wood_pheno_table_V2.csv")
versionone <- read_csv("C:/Users/world/Desktop/Github/growth_phenology/Data/Wood_pheno_table_V1.csv")

unique(Wood_pheno_table$sp)
mean(Wood_pheno_table$max_rate_DOY) #V1: June 7th, V2: June 7th
#mean(versionone$max_rate_DOY) #V1: June 7th, V2: June 7th

boxplot(Wood_pheno_table$max_rate_DOY~Wood_pheno_table$wood_type)
aggregate(Wood_pheno_table$max_rate_DOY, by = list(Wood_pheno_table$wood_type), FUN = mean )
#25%
twentyfive <- subset(Wood_pheno_table, perc == .25)# & sp == "litu")
boxplot(as.numeric(twentyfive$max_rate_DOY)~twentyfive$sp)
boxplot(as.numeric(twentyfive$DOY)~twentyfive$sp)
boxplot(as.numeric(twentyfive$DOY)~twentyfive$wood_type)
mean(as.numeric(twentyfive$DOY))
sd(as.numeric(twentyfive$DOY))
hist(as.numeric(twentyfive$DOY), breaks = 11, xlab = "DOY", main = "DOY where 25% growth achieved")
#
qual25 <- subset(twentyfive, sp == "qual")
plot(as.numeric(qual25$DOY)~as.numeric(qual25$dbh))
boxplot(as.numeric(qual25$DOY)~qual25$year, xlab = "Year", ylab = "DOY", main = "qual 25% growth")

quru25 <- subset(twentyfive, sp == "quru")
plot(as.numeric(quru25$DOY)~as.numeric(quru25$dbh))
boxplot(as.numeric(quru25$DOY)~quru25$year, xlab = "Year", ylab = "DOY", main = "quru 25% growth")

litu25 <- subset(twentyfive, sp == "litu")
plot(as.numeric(litu25$DOY)~as.numeric(litu25$dbh))
boxplot(as.numeric(litu25$DOY)~litu25$year, xlab = "Year", ylab = "DOY", main = "litu 25% growth")

caco25 <- subset(twentyfive, sp == "caco")
plot(as.numeric(caco25$DOY)~as.numeric(caco25$dbh))
boxplot(as.numeric(caco25$DOY)~caco25$year, xlab = "Year", ylab = "DOY", main = "caco 25% growth")

fagr25 <- subset(twentyfive, sp == "fagr")
plot(as.numeric(fagr25$DOY)~as.numeric(fagr25$dbh))
boxplot(as.numeric(fagr25$DOY)~fagr25$year, xlab = "Year", ylab = "DOY", main = "fagr 25% growth")

juni25 <- subset(twentyfive, sp == "juni")
plot(as.numeric(juni25$DOY)~as.numeric(juni25$dbh))
boxplot(as.numeric(juni25$DOY)~juni25$year, xlab = "Year", ylab = "DOY", main = "juni 25% growth")

tiam25 <- subset(twentyfive, sp == "tiam")
plot(as.numeric(tiam25$DOY)~as.numeric(tiam25$dbh))
boxplot(as.numeric(tiam25$DOY)~tiam25$year , xlab = "Year", ylab = "DOY", main = "tiam 25% growth")

unique(all_stems$sp)
#caca, cofl, nysy

#50%
fifty <- subset(Wood_pheno_table, perc == .50)# & sp == "litu")
boxplot(as.numeric(fifty$DOY)~fifty$sp)
boxplot(as.numeric(fifty$DOY)~fifty$wood_type)
mean(as.numeric(fifty$DOY))
sd(as.numeric(fifty$DOY))
hist(as.numeric(fifty$DOY), breaks = 11, xlab = "DOY", main = "DOY where 50% growth achieved")
#
qual50 <- subset(fifty, sp == "qual")
plot(as.numeric(qual50$DOY)~as.numeric(qual50$dbh))
boxplot(as.numeric(qual50$DOY)~qual50$year, xlab = "Year", ylab = "DOY", main = "qual 50% growth")

quru50 <- subset(fifty, sp == "quru")
plot(as.numeric(quru50$DOY)~as.numeric(quru50$dbh))
boxplot(as.numeric(quru50$DOY)~quru50$year, xlab = "Year", ylab = "DOY", main = "quru 50% growth")

litu50 <- subset(fifty, sp == "litu")
plot(as.numeric(litu50$DOY)~as.numeric(litu50$dbh))
boxplot(as.numeric(litu50$DOY)~litu50$year, xlab = "Year", ylab = "DOY", main = "litu 50% growth")

fagr50 <- subset(fifty, sp == "fagr")
plot(as.numeric(fagr50$DOY)~as.numeric(fagr50$dbh))
boxplot(as.numeric(fagr50$DOY)~fagr50$year, xlab = "Year", ylab = "DOY", main = "fagr 50% growth")

caco50 <- subset(fifty, sp == "caco")
plot(as.numeric(caco50$DOY)~as.numeric(caco50$dbh))
boxplot(as.numeric(caco50$DOY)~caco50$year, xlab = "Year", ylab = "DOY", main = "caco 50% growth")

juni50 <- subset(fifty, sp == "juni")
plot(as.numeric(juni50$DOY)~as.numeric(juni50$dbh))
boxplot(as.numeric(juni50$DOY)~juni50$year, xlab = "Year", ylab = "DOY", main = "juni 50% growth")

tiam50 <- subset(fifty, sp == "tiam")
plot(as.numeric(tiam50$DOY)~as.numeric(tiam50$dbh))
boxplot(as.numeric(tiam50$DOY)~tiam50$year , xlab = "Year", ylab = "DOY", main = "tiam 50% growth")
#75%
seventyfive <- subset(Wood_pheno_table, perc == .75)# & sp == "litu")
boxplot(as.numeric(seventyfive$DOY)~seventyfive$sp)
boxplot(as.numeric(seventyfive$DOY)~seventyfive$wood_type)
mean(as.numeric(seventyfive$DOY))
sd(as.numeric(seventyfive$DOY))
hist(as.numeric(seventyfive$DOY), breaks = 11, xlab = "DOY", main = "DOY where 75% growth achieved")
#
qual75 <- subset(seventyfive, sp == "qual")
plot(as.numeric(qual75$DOY)~as.numeric(qual75$dbh))
boxplot(as.numeric(qual75$DOY)~qual75$year, xlab = "Year", ylab = "DOY", main = "qual 75% growth")

quru75 <- subset(seventyfive, sp == "quru")
plot(as.numeric(quru75$DOY)~as.numeric(quru75$dbh))
boxplot(as.numeric(quru75$DOY)~quru75$year, xlab = "Year", ylab = "DOY", main = "quru 75% growth")

litu75 <- subset(seventyfive, sp == "litu")
plot(as.numeric(litu75$DOY)~as.numeric(litu75$dbh))
boxplot(as.numeric(litu75$DOY)~litu75$year, xlab = "Year", ylab = "DOY", main = "litu 75% growth")

fagr75 <- subset(seventyfive, sp == "fagr")
plot(as.numeric(fagr75$DOY)~as.numeric(fagr75$dbh))
boxplot(as.numeric(fagr75$DOY)~fagr75$year, xlab = "Year", ylab = "DOY", main = "fagr 75% growth")

caco75 <- subset(seventyfive, sp == "caco")
plot(as.numeric(caco75$DOY)~as.numeric(caco75$dbh))
boxplot(as.numeric(caco75$DOY)~caco75$year, xlab = "Year", ylab = "DOY", main = "caco 75% growth")

juni75 <- subset(seventyfive, sp == "juni")
plot(as.numeric(juni75$DOY)~as.numeric(juni75$dbh))
boxplot(as.numeric(juni75$DOY)~juni75$year, xlab = "Year", ylab = "DOY", main = "juni 75% growth")

tiam75 <- subset(seventyfive, sp == "tiam")
plot(as.numeric(tiam75$DOY)~as.numeric(tiam75$dbh))
boxplot(as.numeric(tiam75$DOY)~tiam75$year , xlab = "Year", ylab = "DOY", main = "tiam 75% growth")
