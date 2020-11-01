#Calculate Climate moisture index (CMI: Monthly precip - PET)
library(readr)
library(dplyr)
library(tidyverse)
library(reshape2)
pet <- read_csv("C:/Users/world/Desktop/Github/Climatenew/Climate_Data/CRU/CRU_v4_04/pet.1901.2019-ForestGEO_sites-6-03.csv")
pre <- read_csv("C:/Users/world/Desktop/Github/Climatenew/Climate_Data/CRU/CRU_v4_04/pre.1901.2019-ForestGEO_sites-6-03.csv")

#### HF ----
HF_pet <- pet[pet$sites.sitename == "Harvard_Forest",]
HF_pet <- HF_pet %>% select(colnames(HF_pet[grepl("1998|1999|2000|2001|2002|2003", colnames(HF_pet))]))
HF_pet_melt <- HF_pet %>%
  melt()
names(HF_pet_melt) <- c("XDATE", "PET")

HF_pre <- pre[pre$sites.sitename == "Harvard_Forest",]
HF_pre <- HF_pre %>% select(colnames(HF_pre[grepl("1998|1999|2000|2001|2002|2003", colnames(HF_pre))]))
HF_pre_melt <- HF_pre %>%
  melt()
names(HF_pre_melt) <- c("XDATE", "PRE")

HF_CMI <- left_join(HF_pet_melt, HF_pre_melt)
HF_CMI$CMI <- HF_CMI$PRE-HF_CMI$PET

HF_CMI$year <- substr(HF_CMI$XDATE,2, 5)
HF_CMI$month <- substr(HF_CMI$XDATE,7, 8)
HF_CMI$day <- substr(HF_CMI$XDATE,10,11)

HF_CMI <- HF_CMI[complete.cases(HF_CMI$CMI),]

HF_CMI$DATE <- paste(HF_CMI$day, HF_CMI$month, HF_CMI$year, sep = "/")
HF_CMI$DATE <- strptime(as.character(HF_CMI$DATE), format = "%d/%m/%Y")
HF_CMI$DATE <- format(HF_CMI$DATE, "%d/%m/%Y")

write.csv(HF_CMI, file = "HF_CMI.csv", row.names = FALSE)

####SCBI ----
SCBI_pet <- pet[pet$sites.sitename == "Smithsonian_Conservation_Biology_Institute",]
SCBI_pet <- SCBI_pet %>% select(colnames(SCBI_pet[grepl("2011|2012|2013|2014|2015|2016|2017|2018|2019", colnames(SCBI_pet))]))
SCBI_pet_melt <- SCBI_pet %>%
  melt()
names(SCBI_pet_melt) <- c("XDATE", "PET")

SCBI_pre <- pre[pre$sites.sitename == "Smithsonian_Conservation_Biology_Institute",]
SCBI_pre <- SCBI_pre %>% select(colnames(SCBI_pre[grepl("2011|2012|2013|2014|2015|2016|2017|2018|2019", colnames(SCBI_pre))]))
SCBI_pre_melt <- SCBI_pre %>%
  melt()
names(SCBI_pre_melt) <- c("XDATE", "PRE")

SCBI_CMI <- left_join(SCBI_pet_melt, SCBI_pre_melt)
SCBI_CMI$CMI <- SCBI_CMI$PRE-SCBI_CMI$PET

SCBI_CMI$year <- substr(SCBI_CMI$XDATE,2, 5)
SCBI_CMI$month <- substr(SCBI_CMI$XDATE,7, 8)
SCBI_CMI$day <- substr(SCBI_CMI$XDATE,10,11)

SCBI_CMI <- SCBI_CMI[complete.cases(SCBI_CMI$CMI),]

SCBI_CMI$DATE <- paste(SCBI_CMI$day, SCBI_CMI$month, SCBI_CMI$year, sep = "/")
SCBI_CMI$DATE <- strptime(as.character(SCBI_CMI$DATE), format = "%d/%m/%Y")
SCBI_CMI$DATE <- format(SCBI_CMI$DATE, "%d/%m/%Y")

write.csv(SCBI_CMI, file = "SCBI_CMI.csv", row.names = FALSE)

#Quick look
wood_pheno_table_hf <- read_csv("data/Wood_pheno_table_HarvardForest_V4CLEAN.csv")
wood_pheno_table_scbi <- read.csv("data/Wood_pheno_table_V9CLEAN.csv")
HF_agg <- aggregate(HF_CMI$CMI, by = list(HF_CMI$year), FUN = mean)
names(HF_agg) <- c("year","CMI")
HF_agg$year <- as.numeric(HF_agg$year)
SCBI_agg <- aggregate(SCBI_CMI$CMI, by = list(SCBI_CMI$year), FUN = mean)
names(SCBI_agg) <- c("year", "CMI")
SCBI_agg$year <- as.numeric(SCBI_agg$year)

wood_pheno_table_hf <- left_join(wood_pheno_table_hf, HF_agg, by = "year")
Wood_pheno_table_scbi <- left_join(wood_pheno_table_scbi, SCBI_agg, by = "year")

HF_25 <- subset(wood_pheno_table_hf, perc == .25)
HF_50 <- subset(wood_pheno_table_hf, perc == .50)
HF_75 <- subset(wood_pheno_table_hf, perc == .75)

ggplot(HF_25, aes(x= year, y = DOY, group = wood_type, color = wood_type))+geom_point()+
  geom_line(aes(x = year, y = CMI))

SCBI_25 <- subset(wood_pheno_table_scbi, perc == .25)
SCBI_50 <- subset(wood_pheno_table_scbi, perc == .50)
SCBI_75 <- subset(wood_pheno_table_scbi, perc == .75)

ggplot(SCBI_25, aes(x= year, y = DOY, group = wood_type, color = wood_type))+geom_point()+
  geom_line(aes(x = year, y = CMI))

library(lme4)
library(lmerTest)
summary(lmer(DOY ~ CMI*wood_type + (1|sp/tag), data= wood_pheno_table_hf))
summary(lmer(DOY ~ CMI*wood_type + (1|sp/tag), data= wood_pheno_table_scbi))


highestRP_hf <- subset(Wood_pheno_table_hf, year == 2003 & wood_type == "ring-porous")
highestDP_hf <- subset(Wood_pheno_table_hf, year == 2003 & wood_type == "diffuse-porous")
highest_hf <- rbind(highestRP_hf, highestDP_hf)
lowestRP_hf <- subset(Wood_pheno_table_hf, year == 2001 & wood_type == "ring-porous")
lowestDP_hf <- subset(Wood_pheno_table_hf, year == 2001 & wood_type == "diffuse-porous")
lowest_hf <- rbind(lowestDP_hf, lowestRP_hf)
aggregates_hf <- aggregate(Wood_pheno_table_hf$DOY, by = list(Wood_pheno_table_hf$wood_type, Wood_pheno_table_hf$perc), FUN = mean)
aggregates_hf$pre_type <- "Average"
aggregates_high_hf <- aggregate(highest_hf$DOY, by = list(highest_hf$wood_type, highest_hf$perc), FUN = mean)
names(aggregates_high_hf) <- c("Group.1", "Group.2", "x")
aggregates_high_hf$pre_type <- "Wettest Year"
aggregates_low_hf <- aggregate(lowest_hf$DOY, by = list(lowest_hf$wood_type, lowest_hf$perc), FUN = mean)
names(aggregates_low_hf) <- c("Group.1", "Group.2", "x")
aggregates_low_hf$pre_type <- "Driest Year"

aggregates_hf <- rbind(aggregates_hf, aggregates_low_hf, aggregates_high_hf)
aggregates_hf <- aggregates_hf %>% mutate(pre_type = factor(pre_type, levels = c("Wettest Year", "Average", "Driest Year")))

doytiming_hf_CMI <- ggplot(aggregates_hf, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, pre_type), color = pre_type, linetype = Group.1))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "Harvard Forest Intraannual Growth Timing", color = "Wood Type")+
  scale_colour_manual(values = c("red", "purple", "blue"))
###
#SCBI
###
highestRP_scbi <- subset(Wood_pheno_table_scbi, year == 2018 & wood_type == "ring-porous")
highestDP_scbi <- subset(Wood_pheno_table_scbi, year == 2018 & wood_type == "diffuse-porous")
highest_scbi <- rbind(highestRP_scbi, highestDP_scbi)
lowestRP_scbi <- subset(Wood_pheno_table_scbi, year == 2012 & wood_type == "ring-porous")
lowestDP_scbi <- subset(Wood_pheno_table_scbi, year == 2012 & wood_type == "diffuse-porous")
lowest_scbi <- rbind(lowestDP_scbi, lowestRP_scbi)
aggregates_scbi <- aggregate(Wood_pheno_table_scbi$DOY, by = list(Wood_pheno_table_scbi$wood_type, Wood_pheno_table_scbi$perc), FUN = mean)
aggregates_scbi$pre_type <- "Average"
aggregates_high_scbi <- aggregate(highest_scbi$DOY, by = list(highest_scbi$wood_type, highest_scbi$perc), FUN = mean)
names(aggregates_high_scbi) <- c("Group.1", "Group.2", "x")
aggregates_high_scbi$pre_type <- "Wettest Year"
aggregates_low_scbi <- aggregate(lowest_scbi$DOY, by = list(lowest_scbi$wood_type, lowest_scbi$perc), FUN = mean)
names(aggregates_low_scbi) <- c("Group.1", "Group.2", "x")
aggregates_low_scbi$pre_type <- "Driest Year"

aggregates_scbi <- rbind(aggregates_scbi, aggregates_low_scbi, aggregates_high_scbi)
aggregates_scbi <- aggregates_scbi %>% mutate(pre_type = factor(pre_type, levels = c("Wettest Year", "Average", "Driest Year")))

doytiming_scbi_CMI <- ggplot(aggregates_scbi, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, pre_type), color = pre_type, linetype = Group.1))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "Harvard Forest Intraannual Growth Timing", color = "Wood Type")+
  scale_colour_manual(values = c("red", "purple", "blue"))

##Corrlation between temp and CMI
weatherdata_hf <-
  read_csv("climate data/HF_weatherdata.csv") %>%
  filter(!is.na(airtmax))
weatherdata_scbi <-
  read_csv("climate data/met_tower_data_sensor2_ncdc_supplemented.csv") %>%
  filter(!is.na(cleantmax)) %>%
  mutate(year = year.x)

SCBI_CMI <- read_csv("SCBI_CMI.csv") %>%
  mutate(
    month = as.numeric(month)
  )
HF_CMI <- read_csv("HF_CMI.csv") %>%
  mutate(
    month = as.numeric(month)
  )

hf_monthly <- aggregate(weatherdata_hf$airtmax, by = list(weatherdata_hf$month, weatherdata_hf$year), FUN = mean)
names(hf_monthly) <- c("month", "year","temp")
hf_monthly <- left_join(hf_monthly, HF_CMI, by = c("year","month"))

plot(hf_monthly$temp~hf_monthly$CMI)
plot(hf_monthly$temp~hf_monthly$PRE)
plot(hf_monthly$temp~hf_monthly$PET)

scbi_monthly <- aggregate(weatherdata_scbi$cleantmax, by = list(weatherdata_scbi$month, weatherdata_scbi$year), FUN = mean)
names(scbi_monthly) <- c("month", "year","temp")
scbi_monthly <- left_join(scbi_monthly, SCBI_CMI, by = c("year","month"))

plot(scbi_monthly$temp~scbi_monthly$CMI)
plot(scbi_monthly$temp~scbi_monthly$PRE)
plot(scbi_monthly$temp~scbi_monthly$PET)

###Precip totals
pre_hf <- aggregate(HF_CMI$PRE, by = list(HF_CMI$year), FUN = sum)
pre_scbi <- aggregate(SCBI_CMI$PRE, by = list(SCBI_CMI$year), FUN = sum)
