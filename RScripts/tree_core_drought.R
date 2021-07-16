#Script to analyze the joint effect of spring T and summer drought on tree growth using tree cores#
#Written by: Cameron Dow###########################################################################
#Last edit: 6/30/2021##############################################################################
#Issues: 1.Ending left join isnt working for some sites - SCBI and HF arnt in the crns used here. Need to add them, probably manually and create a new file with them all merged
#2. Some sites missing from "toadd" dataframe

rm(list = ls())

library(tidyverse)
library(readr)
library(lme4)
library(lmerTest)
library(emmeans)
library(reshape2)
library(readxl)
crns <- read_csv("Data/tree_rings/Other/all_crns_res_1901.csv") %>%
  # clean up
  select(-c(BearIs, OH_Gol_QUAL_1, PineMt_QUMO)) %>%
  rename(IL_Fer_LITU = IL_Fer_LTU,
         IL_Bea_QURU = IL_BeaQURU)

crns_long <- crns %>%
  # convert to long format
  pivot_longer(-Year, names_to = "site_sp", values_to = "ring_width") %>%
  # drop years with missing data
  filter(!is.na(ring_width))

crns_long$Location <- substr(crns_long$site_sp, 1,nchar(crns_long$site_sp)-5)

#Add lat / lon
TRW_coord <- read_excel("Data/tree_rings/Other/TRW_coord2.xlsx")
TRW_coord <- TRW_coord[,c(1,2,3)]
#Add original two locations to include in final quilt plot
originals <- data.frame(42.5388, -72.18, "HF") #Lat for HF = 42.5388
originals <- rbind(originals, c(38.8935, -78.1454, "SCBI")) #Lat for scbi = 38.8935
names(originals) <- c("Latitude","Longitude", "Location")
TRW_coord <- rbind(TRW_coord, originals)

crns_long_try <- left_join(crns_long, TRW_coord, by = "Location")

#replace na's with correct values somehow....no good way to do it. manually change csv.
write.csv(crns_long_try, file = "crns_long_try.csv", row.names = FALSE)
# crns_long_try$Latitude <- ifelse(is.na(crns_long_try$Latitude), "37.7", crns_long_try$Latitude)
# is.na(crns_long_try$Latitude) <- "37.7"
# Load climate data ####
crns_long <- read.csv("crns_long_try.csv")

## needs to be in different format: column for Date, year and then one column per climate variable
climate_variables <- c("tmn", "tmx")
clim_v <- NULL
# something like this should do
for(clim_v in climate_variables) {
  print(clim_v)
  x <- read.csv(paste0("climate data/CRU/", clim_v,  ".1901.2019-Other_sites-3-11.csv"))


  ### subset for the sites we care about

  #x <- droplevels(x[x$sites.sitename %in% "Harvard_Forest", ])

  ### reshape to long format
  x_long <- reshape(x,
                    times = names(x)[-1],
                    timevar = "Date",
                    varying = list(names(x)[-1]),
                    direction = "long", v.names = clim_v)
  ### format date
  x_long$Date <- gsub("X", "", x_long$Date)
  x_long$Date <- as.Date(x_long$Date , format = "%Y.%m.%d")#changed format to work with Harvard data


  ### combine all variables in one
  if(clim_v == climate_variables[1]) all_Clim <- x_long[, c(1:3)]
  else all_Clim <- merge(all_Clim, x_long[, c(1:3)], by = c("sites.sitename", "Date"), all = T)

}
### add year column
all_Clim$Year <- as.numeric(format(as.Date(all_Clim$Date, format = "%d/%m/%Y"), "%Y"))
### add month column
all_Clim$month <- as.numeric(format(as.Date(all_Clim$Date, format = "%d/%m/%Y"), "%m"))

april_means <- all_Clim %>%
  filter(month == 4) %>%
  group_by(sites.sitename,Year) %>%
  summarize(tmx = mean(tmx)) %>%
  rename(Location = sites.sitename,
         april = tmx)

june_means <- all_Clim %>%
  filter(month == 6) %>%
  group_by(sites.sitename,Year) %>%
  summarize(tmx = mean(tmx)) %>%
  rename(Location = sites.sitename,
         june = tmx)

june_july_means <- all_Clim %>%
  filter(month %in% c(6,7)) %>%
  group_by(sites.sitename,Year) %>%
  summarize(tmx = mean(tmx)) %>%
  rename(Location = sites.sitename,
         june_july = tmx)

may_aug_means <- all_Clim %>%
  filter(month %in% c(5,6,7,8)) %>%
  group_by(sites.sitename,Year) %>%
  summarize(tmx = mean(tmx)) %>%
  rename(Location = sites.sitename,
         may_aug = tmx)

crns_long <- left_join(crns_long, april_means, by = c("Location", "Year"))
crns_long <- left_join(crns_long, june_means, by = c("Location", "Year"))
crns_long <- left_join(crns_long, june_july_means, by = c("Location", "Year"))
crns_long <- left_join(crns_long, may_aug_means, by = c("Location", "Year"))

species_all <- unique(crns_long$site_sp)
p_vals <- data.frame(1,1,1,1)
names(p_vals) <- c("cur_sp", "variable", "p-value", "est")
p_vals <- p_vals[-1,]
#For loop runs through LM's for each clim variable and captures p-vals in one DF
for(i in 1:length(species_all)){
cur_sp <- species_all[i]
crns_sub <- crns_long[crns_long$site_sp %in% cur_sp,]

a <- summary(lm(ring_width ~ april + april:june, data = crns_sub))
apr_jun <- a[["coefficients"]][3,4]
apr_jun_est <- a[["coefficients"]][3,1]

b <- summary(lm(ring_width ~ april + april:june_july, data = crns_sub))
apr_jun_jul <- b[["coefficients"]][3,4]
apr_jun_jul_est <- b[["coefficients"]][3,1]

c <- summary(lm(ring_width ~ april + april:may_aug, data = crns_sub))
apr_summer <- c[["coefficients"]][3,4]
apr_summer_est <- c[["coefficients"]][3,1]

est <- c(apr_jun_est, apr_jun_jul_est, apr_summer_est)

current_loop <- data.frame(cur_sp, apr_jun, apr_jun_jul, apr_summer)
melt_cur <- melt(current_loop)
names(melt_cur) <- c("cur_sp", "variable", "p-value")

latlon <- crns_sub[c(1:3),c(5,6)]

melt_cur <- cbind(melt_cur, est, latlon)
p_vals <- rbind(p_vals, melt_cur)
}

write.csv(p_vals, file = "Data/Tree_core_drought.csv", row.names = FALSE)

#Add sigs to chronology table
rm(list = ls())

toadd <- read_csv("Data/Tree_core_drought.csv")

#convert from long to wide so columns can be appended to chrono table
toadd <- pivot_wider(data=toadd, id_cols = c(cur_sp), names_from = variable, values_from = c(`p-value`, est,Latitude, Longitude))
toadd <- toadd[,c(1,11,8,2:7)]
#rename lat and lon columns
toadd <- toadd %>%
  rename(Latitude = Latitude_apr_jun,
         Longitude = Longitude_apr_jun)

#Create sp code column for merge
toadd$sp <- substr(toadd$cur_sp, nchar(toadd$cur_sp)-3, nchar(toadd$cur_sp))
#Create unique code column using lat, lon, and sp code for each entry
toadd$ID <- paste0(toadd$Latitude, toadd$Longitude, toadd$sp)

#Read in sp code csv to turn latin names in chrono table to sp code
sp_codes <- read_csv("C:/Users/world/Desktop/sp_codes.csv")
sp_codes <- sp_codes[,c(1,2)]
names(sp_codes) <- c("sp", "Species")

#read in chronology table csv which we are appending the lm results to
chronology_table <- read.csv("doc/manuscript/tables_figures/chronology_table.csv")
#Add sp codes to chrono table for unique ID's
chronology_table <- left_join(chronology_table, sp_codes, by = "Species")
chronology_table$ID <- paste0(chronology_table$Latitude, chronology_table$Longitude,chronology_table$sp)

#Left join the two to get LM results appended to chronology table
attempt_3 <- left_join(chronology_table,toadd, by = "ID")
attempt_4 <- attempt_3[!(duplicated(attempt_3$group)),]#Two sites are duplicated
attempt_4 <- attempt_4[,c(1:8,14:19)]#remove unnecessary columns

#Rename columns to something understandable
names(attempt_4) <- c("Number", "Location", "Species", "Wood Type", "CTW Tmin", "CTW Tmax", "Latitude", "Longitude", "P-value April:June Tmax", "P-value April:June-July Tmax", "P-value April:May-August Tmax", "Slope April:June Tmax", "Slope April:June-July Tmax","Slope April:May-August Tmax")
#Rearrange so slopes are followed by their p-values
attempt_4 <- attempt_4[,c(1:8,12,9,13,10,14,11)]

write.csv(attempt_4, file = "doc/manuscript/tables_figures/chronology_table_drought_added.csv", row.names = FALSE)
