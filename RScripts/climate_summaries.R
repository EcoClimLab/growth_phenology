#Compare our study period to the past 30 years. SPEI, Summer temps, Spring temps
library(tidyverse)
library(reshape2)
library(lubridate)
tmp <- read_csv("Data/climate data/CRU/tmx.1901.2019-all_sites-3-11.csv")
scbi_tmp <- tmp[tmp$sites.sitename %in% "SCBI",]
scbi_tmp <- melt(scbi_tmp, id.vars = "sites.sitename")
scbi_tmp$variable <- substr(scbi_tmp$variable, 2, nchar(as.character(scbi_tmp$variable)) )

scbi_2020 <- met_tower_data_sensor2_ncdc_supplemented <- read_csv("Data/climate data/SCBI/met_tower_data_sensor2_ncdc_supplemented.csv")
scbi_2020 <- scbi_2020[scbi_2020$year.x %in% 2020,]
monthly_2020 <- aggregate(scbi_2020[!is.na(scbi_2020$cleantmax),]$cleantmax, by = list(scbi_2020[!is.na(scbi_2020$cleantmax),]$month), FUN = mean)

scbi_tmp$variable <- as.Date(scbi_tmp$variable, format = "%Y.%m.%d")
scbi_tmp$year <- year(scbi_tmp$variable)
scbi_tmp$month <- month(scbi_tmp$variable)

scbi_tmp <- scbi_tmp[scbi_tmp$year %in% c(1970:2019),]
scbi_tmp <- scbi_tmp[!is.na(scbi_tmp$value),]
scbi_tmp <- scbi_tmp[scbi_tmp$month %in% c(3,4),]
scbi_tmp <- scbi_tmp[,c(3,4,5)]

monthly_2020 <- monthly_2020[monthly_2020$Group.1 %in% c(3,4),]
clim_2020_3 <- data.frame(monthly_2020[1,2], 2020, 3 )
names(clim_2020_3) <- names(scbi_tmp)
clim_2020_4 <- data.frame(monthly_2020[2,2], 2020, 4 )
names(clim_2020_4) <- names(scbi_tmp)

clim_2020 <- rbind(clim_2020_3,clim_2020_4)
scbi_tmp <- rbind(scbi_tmp, clim_2020)
mean(scbi_tmp[scbi_tmp$year %in% c(1970:2010),]$value)
mean(scbi_tmp[scbi_tmp$year %in% c(2010:2019),]$value)#going to 2019 because I don't have easily accessible SPEI data for 2020


#SPEI
spei <- read_csv("Data/climate data/CRU/spei_all_months.csv")

spei <- spei[spei$sites.sitename %in% c("Smithsonian_Conservation_Biology_Institute"),]
spei$Date <- as.Date(spei$Date, format = "%m/%d/%Y")
spei$year <- year(spei$Date)
spei$month <- month(spei$Date)

spei <- spei[spei$year %in% c(1970:2019),]

spei<- spei[,c(1,6,8,14,51,52)]

#Jun Jul Aug, 4,6,12
#June
june <- spei[spei$month %in% 6,]
mean(june[june$year %in% c(1970:2011),]$value_month_4)
mean(june[june$year %in% c(2011:2019),]$value_month_4)

mean(june[june$year %in% c(1970:2011),]$value_month_6)
mean(june[june$year %in% c(2011:2019),]$value_month_6)

mean(june[june$year %in% c(1970:2011),]$value_month_12)
mean(june[june$year %in% c(2011:2019),]$value_month_12)

june[june$year %in% c(2012),]$value_month_4

#july
july <- spei[spei$month %in% 7,]
mean(july[july$year %in% c(1970:2011),]$value_month_4)
mean(july[july$year %in% c(2011:2019),]$value_month_4)

mean(july[july$year %in% c(1970:2011),]$value_month_6)
mean(july[july$year %in% c(2011:2019),]$value_month_6)

mean(july[july$year %in% c(1970:2011),]$value_month_12)
mean(july[july$year %in% c(2011:2019),]$value_month_12)

july[july$year %in% c(2012),]$value_month_4

#august
august <- spei[spei$month %in% 8,]
mean(august[august$year %in% c(1970:2010),]$value_month_4)
mean(august[august$year %in% c(2011:2019),]$value_month_4)

mean(august[august$year %in% c(1970:2011),]$value_month_6)
mean(august[august$year %in% c(2011:2019),]$value_month_6)

mean(august[august$year %in% c(1970:2011),]$value_month_12)
mean(august[august$year %in% c(2011:2019),]$value_month_12)

august[august$year %in% c(2012),]$value_month_4


##############################################################
#Harvard Forest
tmp <- read_csv("Data/climate data/CRU/tmx.1901.2019-all_sites-3-11.csv")
hf_tmp <- tmp[tmp$sites.sitename %in% "HF_LyfordPlots",]
hf_tmp <- melt(hf_tmp, id.vars = "sites.sitename")

hf_tmp$variable <- substr(hf_tmp$variable, 2, nchar(as.character(hf_tmp$variable)) )

hf_tmp$variable <- as.Date(hf_tmp$variable, format = "%Y.%m.%d")
hf_tmp$year <- year(hf_tmp$variable)
hf_tmp$month <- month(hf_tmp$variable)

hf_tmp <- hf_tmp[hf_tmp$year %in% c(1970:2019),]
hf_tmp <- hf_tmp[!is.na(hf_tmp$value),]

hf_tmp_spring <- hf_tmp[hf_tmp$month %in% c(3,4),]
mean(hf_tmp_spring$value)
mean(hf_tmp_spring[hf_tmp_spring$year %in% c(1970:1998),]$value)
mean(hf_tmp_spring[hf_tmp_spring$year %in% c(1999:2003),]$value)

hf_tmp_summer <- hf_tmp[hf_tmp$month %in% c(6,7,8),]
mean(hf_tmp_summer$value)
mean(hf_tmp_summer[hf_tmp_summer$year %in% c(1970:1998),]$value)
mean(hf_tmp_summer[hf_tmp_summer$year %in% c(1999:2003),]$value)

hf_tmp_fall <- hf_tmp[hf_tmp$month %in% c(9,10,11),]
mean(hf_tmp_fall$value)
mean(hf_tmp_fall[hf_tmp_fall$year %in% c(1970:1998),]$value)
mean(hf_tmp_fall[hf_tmp_fall$year %in% c(1999:2003),]$value)

hf_tmp_winter <- hf_tmp[hf_tmp$month %in% c(1,2),]
mean(hf_tmp_winter$value)
mean(hf_tmp_winter[hf_tmp_winter$year %in% c(1970:1998),]$value)
mean(hf_tmp_winter[hf_tmp_winter$year %in% c(1999:2003),]$value)

#SPEI
spei <- read_csv("Data/climate data/CRU/spei_all_months.csv")

spei <- spei[spei$sites.sitename %in% c("Harvard_Forest"),]
spei$Date <- as.Date(spei$Date, format = "%m/%d/%Y")
spei$year <- year(spei$Date)
spei$month <- month(spei$Date)

spei <- spei[spei$year %in% c(1970:2019),]

spei<- spei[,c(1,6,8,14,51,52)]

#Jun Jul Aug, 4,6,12
#June
june <- spei[spei$month %in% 6,]
mean(june[june$year %in% c(1970:1999),]$value_month_4)
mean(june[june$year %in% c(1999:2003),]$value_month_4)

mean(june[june$year %in% c(1970:1999),]$value_month_6)
mean(june[june$year %in% c(1999:2003),]$value_month_6)

mean(june[june$year %in% c(1970:1999),]$value_month_12)
mean(june[june$year %in% c(1999:2003),]$value_month_12)

june[june$year %in% c(1999),]$value_month_4

#july
july <- spei[spei$month %in% 7,]
mean(july[july$year %in% c(1970:1999),]$value_month_4)
mean(july[july$year %in% c(1999:2003),]$value_month_4)

mean(july[july$year %in% c(1970:1999),]$value_month_6)
mean(july[july$year %in% c(1999:2003),]$value_month_6)

mean(july[july$year %in% c(1970:1999),]$value_month_12)
mean(july[july$year %in% c(1999:2003),]$value_month_12)

july[july$year %in% c(1999),]$value_month_4

#august
august <- spei[spei$month %in% 8,]
mean(august[august$year %in% c(1970:1999),]$value_month_4)
mean(august[august$year %in% c(1999:2003),]$value_month_4)

mean(august[august$year %in% c(1970:1999),]$value_month_6)
mean(august[august$year %in% c(1999:2003),]$value_month_6)

mean(august[august$year %in% c(1970:1999),]$value_month_12)
mean(august[august$year %in% c(1999:2003),]$value_month_12)

august[august$year %in% c(1999),]$value_month_4
