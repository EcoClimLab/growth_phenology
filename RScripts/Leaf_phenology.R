library(readr)
library(lubridate)
library(tidyverse)

pheno <- read_csv("Data/Leaf phenology/scbiPhen.csv")
pheno_hf <- subset(pheno, pointid == "HARV")
pheno_scbi <- subset(pheno, pointid == "SCBI")
pheno_events <- data.frame(1,1,1,1,1,1)
names(pheno_events) <- c("site","year","greenup","midgreenup", "peak","senescence")

for(i in c(1:nrow(pheno_scbi))){
  pheno_events[i,1] <- as.character("SCBI")
  pheno_events[i,2] <- 2000+i
  pheno_events[i,3] <- as.numeric(pheno_scbi[i,6])
  pheno_events[i,4] <- as.numeric(pheno_scbi[i,8])
  pheno_events[i,5] <- as.numeric(pheno_scbi[i,10])
  pheno_events[i,6] <- as.numeric(pheno_scbi[i,14])

}

for(i in c(1:nrow(pheno_hf))){
  pheno_events[i+18,1] <- as.character("HF")
  pheno_events[i+18,2] <- 2000+i
  pheno_events[i+18,3] <- as.numeric(pheno_hf[i,6])
  pheno_events[i+18,4] <- as.numeric(pheno_hf[i,8])
  pheno_events[i+18,5] <- as.numeric(pheno_hf[i,10])
  pheno_events[i+18,6] <- as.numeric(pheno_hf[i,14])

}

pheno_events$los <- pheno_events$senescence-pheno_events$greenup

#Add climate variable
scbi <- read_csv("climate data/scbi_2001to2018.csv")
scbi <- scbi[,c(3,4)]
scbi <- scbi[complete.cases(scbi$TMAX),]
scbi$month <- month(scbi$DATE)
scbi$year <- year(scbi$DATE)
scbi$doy <- yday(scbi$DATE)
scbi_sub <- subset(scbi, doy >= 78 & doy <= 140)
scbi_agg <- aggregate(scbi_sub$TMAX, by = list(scbi_sub$year), FUN = mean)
scbi_agg$site <- "SCBI"
names(scbi_agg) <- c("year","tmp", "site")

HF <- read_csv("climate data/HF_2001toPresent.csv")
HF <- HF[,c(1,5)]

HF$month <- month(HF$date)
HF$year <- year(HF$date)
HF$doy <- yday(HF$date)
HF_sub <- subset(HF, doy >= 78 & doy <= 133)
HF_agg <- aggregate(HF_sub$airtmax, by = list(HF_sub$year), FUN = mean)
HF_agg$site <- "HF"
names(HF_agg) <- c("year","tmp", "site")

temp_variables <- rbind(scbi_agg, HF_agg)
pheno_events <- left_join(pheno_events, temp_variables , by = c("year","site"))

pheno_events$greenup <- as.Date(pheno_events$greenup, origin = "1970-01-01")
pheno_events$midgreenup <- as.Date(pheno_events$midgreenup, origin = "1970-01-01")
pheno_events$peak <- as.Date(pheno_events$peak, origin = "1970-01-01")
pheno_events$senescence <- as.Date(pheno_events$senescence, origin = "1970-01-01")

write.csv(pheno_events, file = "Data/Leaf phenology/leaf_phenology.csv", row.names = FALSE)

#hf <- subset(pheno_events, site == "HF")
#plot(hf$los~hf$tmp)
#abline(lm(hf$los~hf$tmp))
#summary(lm(hf$los~hf$tmp))
