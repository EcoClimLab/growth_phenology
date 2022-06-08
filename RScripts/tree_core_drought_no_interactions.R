#Script to analyze the joint effect of spring T and summer drought on tree growth using tree cores#
#Written by: Cameron Dow###########################################################################
#Last edit: 8/16/2021##############################################################################

rm(list = ls())

library(tidyverse)
library(readr)
library(lme4)
library(lmerTest)
library(emmeans)
library(reshape2)
library(readxl)

#Create crns_long ----
crns <- read_csv("data/tree_rings/chronologies/all_crns_res_1901.csv") %>%
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
TRW_coord <- read_excel("data/tree_rings/TRW_coord2.xlsx")
TRW_coord <- TRW_coord[,c(1,2,3)]
#Add original two locations to include in final quilt plot
originals <- data.frame(42.5388, -72.18, "HF") #Lat for HF = 42.5388
originals <- rbind(originals, c(38.8935, -78.1454, "SCBI")) #Lat for scbi = 38.8935
names(originals) <- c("Latitude","Longitude", "Location")
TRW_coord <- rbind(TRW_coord, originals)
TRW_coord <- TRW_coord[!duplicated(TRW_coord$Location),]
crns_long_try <- left_join(crns_long, TRW_coord, by = "Location")

#replace na's with correct values
crns_long_try[crns_long_try$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_CAOV", "Latitude"] <- "37.7707167"
crns_long_try[crns_long_try$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_CAOV", "Longitude"] <- "-79.2416"

crns_long_try[crns_long_try$Location %in% "Fiddler<U+393C><U+3E32>s_Green_QURU", "Latitude"] <- "37.7694333"
crns_long_try[crns_long_try$Location %in% "Fiddler<U+393C><U+3E32>s_Green_QURU", "Longitude"] <- "-79.2415167"

crns_long_try[crns_long_try$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_LITU", "Latitude"] <- "37.7694333"
crns_long_try[crns_long_try$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_LITU", "Longitude"] <- "-79.2415167"

crns_long_try[crns_long_try$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_MAAC", "Latitude"] <- "37.7686833"
crns_long_try[crns_long_try$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_MAAC", "Longitude"] <- "-79.2423833"

write.csv(crns_long_try, file = "data/tree_rings/chronologies/crns_long.csv", row.names = FALSE)
# crns_long_try$Latitude <- ifelse(is.na(crns_long_try$Latitude), "37.7", crns_long_try$Latitude)
# is.na(crns_long_try$Latitude) <- "37.7"
# Create tree_core_drought data ####
crns_long <- read.csv("data/tree_rings/chronologies/crns_long.csv")

## needs to be in different format: column for Date, year and then one column per climate variable
climate_variables <- c("tmn", "tmx")
clim_v <- NULL
# something like this should do
for(clim_v in climate_variables) {
  print(clim_v)
  x <- read.csv(paste0("Data/climate data/CRU/", clim_v,  ".1901.2019-all_sites-3-11.csv"))


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

crns_long <- crns_long[!is.na(crns_long$april),]

species_all <- unique(crns_long$site_sp)
p_vals <- data.frame(NULL)
rs <- data.frame(NULL)
#names(p_vals) <- c("cur_sp", "variable", "p-value", "est")
#p_vals <- p_vals[-1,]
#For loop runs through LM's for each clim variable and captures p-vals in one DF
for(i in 1:length(species_all)){
  cur_sp <- species_all[i]
  crns_sub <- crns_long[crns_long$site_sp %in% cur_sp,]

  # aa <- summary(lm(ring_width ~ april, data = crns_sub))
  # apr <- aa[["coefficients"]][2,4]
  # apr_est <- aa[["coefficients"]][2,1]
  # apr_r <- aa[["adj.r.squared"]]

  # a <- summary(lm(ring_width ~ april + june + april:june, data = crns_sub))
  # apr_jun_apr <- a[["coefficients"]][2,4]
  # apr_jun_apr_est <- a[["coefficients"]][2,1]
  # apr_jun <- a[["coefficients"]][4,4]
  # apr_jun_est <- a[["coefficients"]][4,1]
  # jun_apr_jun <- a[["coefficients"]][3,4]
  # jun_apr_jun_est <- a[["coefficients"]][3,1]
  # apr_jun_r <- a[["adj.r.squared"]]
  #
  # #Estaban's way!
  # a <- lm(ring_width ~ april + june + april:june, data = crns_sub)
  # coefficients (a) #provides the coefficients of the model (these are conditional coefficients: focus on one variable holding the others constant)
  # summary(a) #provides the F, df, and P value for the whole model (check the last line of the output)
  # Anova(a, type =3) #provides the F, df, and P value for each independent factor
  # anova_stats(a, digits =3) #provides the effect size (we are reporting partial Omega squared)

  # ab <- summary(lm(ring_width ~ june, data = crns_sub))
  # jun <- ab[["coefficients"]][2,4]
  # jun_est <- ab[["coefficients"]][2,1]
  # jun_r <- ab[["adj.r.squared"]]
  #
  # jun_r <- ifelse(jun <= 0.05, apr_r, NA)

  b <- summary(lm(ring_width ~ april + june_july, data = crns_sub))
  apr_jun_jul_apr <- b[["coefficients"]][2,4]
  apr_jun_jul_apr_est <- b[["coefficients"]][2,1]
  jj_apr_jun_jul <- b[["coefficients"]][3,4]
  jj_apr_jun_jul_est <- b[["coefficients"]][3,1]
  #apr_jun_jul <- b[["coefficients"]][4,4]
  #apr_jun_jul_est <- b[["coefficients"]][4,1]
  apr_jun_jul_r <- b[["adj.r.squared"]]

  #Estaban's way!
  # b <- lm(ring_width ~ april + june_july + april:june_july, data = crns_sub)
  # coefficients (b) #provides the coefficients of the model (these are conditional coefficients: focus on one variable holding the others constant)
  # summary(b) #provides the F, df, and P value for the whole model (check the last line of the output)
  # Anova(b, type =3) #provides the F, df, and P value for each independent factor
  # anova_stats(b, digits =3) #provides the effect size (we are reporting partial Omega squared)

  # bb <- summary(lm(ring_width ~ june_july, data = crns_sub))
  # jun_jul <- bb[["coefficients"]][2,4]
  # jun_jul_est <- bb[["coefficients"]][2,1]
  # jun_jul_r <- bb[["adj.r.squared"]]

  # c <- summary(lm(ring_width ~ april + may_aug + april:may_aug, data = crns_sub))
  # apr_summer_apr <- c[["coefficients"]][2,4]
  # apr_summer_apr_est <- c[["coefficients"]][2,1]
  # sum_apr_summer <- c[["coefficients"]][3,4]
  # sum_apr_summer_est <- c[["coefficients"]][3,1]
  # apr_summer <- c[["coefficients"]][4,4]
  # apr_summer_est <- c[["coefficients"]][4,1]
  # apr_summer_r <- c[["adj.r.squared"]]
  #
  # cb <- summary(lm(ring_width ~ may_aug, data = crns_sub))
  # may_aug <- cb[["coefficients"]][2,4]
  # may_aug_est <- cb[["coefficients"]][2,1]
  # may_aug_r <- cb[["adj.r.squared"]]

  est <- c(apr_jun_jul_apr_est, jj_apr_jun_jul_est)

  r_squared <- data.frame(cur_sp,apr_jun_jul_r)
  rs <- rbind(rs, r_squared)

  current_loop <- data.frame(cur_sp, apr_jun_jul_apr,jj_apr_jun_jul)
  melt_cur <- melt(current_loop)
  names(melt_cur) <- c("cur_sp", "variable", "p-value")

  lat <- crns_sub[c(1),c(5)]
  lon <- crns_sub[c(1),c(6)]

  melt_cur <- cbind(melt_cur, est, lat,lon)
  p_vals <- rbind(p_vals, melt_cur)
}


write.csv(p_vals, file = "results/tree_cores/processed_data/Tree_core_drought_nointeraction.csv", row.names = FALSE)

#only sig models -
#sig_only <- p_vals[p_vals$`p-value` >= 0.05,]
#write.csv(sig_only, file = "Data/Significant_Tree_core_drought.csv", row.names = FALSE)

#messing around with r-squared ----
rs$sp <- substr(rs$cur_sp, nchar(rs$cur_sp)-3, nchar(rs$cur_sp))
RP <- c("CAGL","CAOV","CATO","CACO","QURU", "QUST", "QUAL","QUPR","QUMO", "FRAM", "QUVE", "FRNI","QUMA", "QUPA")
SP <- c( "JUNI", "SAAL")
DP <- c("FAGR", "LITU", "MAAC", "ACSA","ACRU", "NYSY","BELE","BEAL", "POGR")

ring <- rs[rs$sp %in% RP ,]
ring <- ring[!duplicated(ring$cur_sp),]
diffuse <- rs[rs$sp %in% DP,]
diffuse <- diffuse[!duplicated(diffuse$cur_sp),]

# for(i in 1:ncol(rs)){
#   ring[143,i] <-  mean(ring[,i], na.rm = TRUE)
#   diffuse[68,i] <- mean(diffuse[,i], na.rm = TRUE)
# }
for(i in 1:ncol(rs)){
  rs[216,i] <- mean(rs[,i], na.rm = TRUE)
}

means <- rs[216,]
ring_melt <- melt(ring, id.vars = c("cur_sp", "sp"))
boxplot(ring_melt$value~ring_melt$variable)
#Add sigs to chronology table----
rm(list = ls())

#Read in Tree core drought data created in last step. Fiddler's green and some HF have their lat/lon manually changed to match the values in the chronology table
toadd <- read_csv("results/tree_cores/processed_data/Tree_core_drought_nointeraction.csv") %>%
  rename(Latitude = lat,
         Longitude = lon)

#convert from long to wide so columns can be appended to chrono table
toadd <- pivot_wider(data=toadd, id_cols = c(cur_sp), names_from = variable, values_from = c(`p-value`, est,Latitude, Longitude))
toadd <- toadd[,c(1:6,8)]
#rename lat and lon columns
toadd <- toadd %>%
  rename(Latitude = Latitude_apr_jun_jul_apr,
         Longitude = Longitude_apr_jun_jul_apr)

#Fix incorrect lat/lon for merging later
toadd[toadd$cur_sp %in% "HF_FRAM", "Latitude"] <- 42.53
toadd[toadd$cur_sp %in% "HF_FRAM", "Longitude"] <- -72.18

toadd[toadd$cur_sp %in% "HF_ACRU", "Latitude"] <- 42.5388
toadd[toadd$cur_sp %in% "HF_ACRU", "Longitude"] <- -72.1755

toadd[toadd$cur_sp %in% "HF_BEAL", "Latitude"] <- 42.5388
toadd[toadd$cur_sp %in% "HF_BEAL", "Longitude"] <- -72.1755

toadd[toadd$cur_sp %in% "HF_QURU", "Latitude"] <- 42.5388
toadd[toadd$cur_sp %in% "HF_QURU", "Longitude"] <- -72.1755

toadd[toadd$cur_sp %in% "HF_LyfordPlots_QURU", "Latitude"] <- 42.5388
toadd[toadd$cur_sp %in% "HF_LyfordPlots_QURU", "Longitude"] <- -72.1755

toadd$Latitude <- round(toadd$Latitude, digits = 7)
toadd$Longitude <- round(toadd$Longitude, digits = 7)
#toadd$Latitude <- ifelse(is.na(toadd$Latitude), 37.7686833, toadd$Latitude)
#toadd$Longitude <- ifelse(is.na(toadd$Longitude), -79.2423833, toadd$Longitude)

#Create sp code column for merge
toadd$sp <- substr(toadd$cur_sp, nchar(toadd$cur_sp)-3, nchar(toadd$cur_sp))
toadd$sp <- ifelse(toadd$sp == "ACSA", "ACSH", toadd$sp)
#Create unique code column using lat, lon, and sp code for each entry
toadd$ID <- paste0(toadd$Latitude, toadd$Longitude, toadd$sp)

#Read in sp code csv to turn latin names in chrono table to sp code
sp_codes <- read_csv("C:/Users/world/Desktop/sp_codes.csv")
sp_codes <- sp_codes[,c(1,2)]
names(sp_codes) <- c("sp", "Species")

#read in chronology table csv which we are appending the lm results to
chronology_table <- read.csv("doc/manuscript/tables_figures/chronology_table.csv")
chronology_table <- chronology_table[,1:8]

chronology_table[chronology_table$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_CAOV", "Latitude"] <- "37.7707167"
chronology_table[chronology_table$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_CAOV", "Longitude"] <- "-79.2416"

chronology_table[chronology_table$Location %in% "Fiddler<U+393C><U+3E32>s_Green_QURU", "Latitude"] <- "37.7694333"
chronology_table[chronology_table$Location %in% "Fiddler<U+393C><U+3E32>s_Green_QURU", "Longitude"] <- "-79.2415167"

chronology_table[chronology_table$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_LITU", "Latitude"] <- "37.7694333"
chronology_table[chronology_table$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_LITU", "Longitude"] <- "-79.2415167"

chronology_table[chronology_table$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_MAAC", "Latitude"] <- "37.7686833"
chronology_table[chronology_table$Location %in% "Fiddler<U+393C><U+3E32>s_Green,_VA_MAAC", "Longitude"] <- "-79.2423833"

chronology_table$Latitude <- round(as.numeric(chronology_table$Latitude), digits = 7)
chronology_table$Longitude <- round(as.numeric(chronology_table$Longitude), digits = 7)

#Add sp codes to chrono table for unique ID's
chronology_table <- left_join(chronology_table, sp_codes, by = "Species")
chronology_table$ID <- paste0(chronology_table$Latitude, chronology_table$Longitude,chronology_table$sp)

#Left join the two to get LM results appended to chronology table
attempt_3 <- left_join(chronology_table,toadd, by = "ID")
attempt_4 <- attempt_3[!(duplicated(attempt_3$Number)),]#Two sites are duplicated
attempt_4 <- attempt_4[,c(1:8,12:15)]#remove unnecessary columns

#Rename columns to something understandable
names(attempt_4) <- c("Number", "Location",
                      "Species",
                      "Wood Type",
                      "CTW Tmin",
                      "CTW Tmax",
                      "Latitude",
                      "Longitude",
                      "P-value April Tmax",
                      "P-value June-July Tmax",
                      "Slope April Tmax",
                      "Slope June-July Tmax"
                      )
#Rearrange so slopes are followed by their p-values
#attempt_4 <- attempt_4[,c(1:8,19, 9, 20, 10, 21, 11, 22 , 12, 23, 13, 24, 14, 25, 15, 26, 16, 27, 17, 28, 18)]
attempt_4 <- attempt_4[,c(1:8, 11, 9, 12, 10)]

#write.csv(attempt_4, file = "doc/manuscript/tables_figures/chronology_table.csv", row.names = FALSE)
write.csv(attempt_4, file = "doc/manuscript/tables_figures/chronology_table.csv", row.names = FALSE)

# Significant models only ----
rm(list = ls())

#Read in Tree core drought data created in last step. Fiddler's green and some HF have their lat/lon manually changed to match the values in the chronology table
toadd_sig <- read_csv("results/tree_cores/processed_data/Significant_Tree_core_drought.csv") %>%
  rename(Latitude = lat,
         Longitude = lon)

#convert from long to wide so columns can be appended to chrono table
toadd <- pivot_wider(data=toadd, id_cols = c(cur_sp), names_from = variable, values_from = c(`p-value`, est,Latitude, Longitude))
toadd <- toadd[,c(1,22,32,2:21)]
#rename lat and lon columns
toadd <- toadd %>%
  rename(Latitude = Latitude_apr,
         Longitude = Longitude_apr)
toadd$Latitude <- round(toadd$Latitude, digits = 7)
toadd$Longitude <- round(toadd$Longitude, digits = 7)
#toadd$Latitude <- ifelse(is.na(toadd$Latitude), 37.7686833, toadd$Latitude)
#toadd$Longitude <- ifelse(is.na(toadd$Longitude), -79.2423833, toadd$Longitude)

#Create sp code column for merge
toadd$sp <- substr(toadd$cur_sp, nchar(toadd$cur_sp)-3, nchar(toadd$cur_sp))
toadd$sp <- ifelse(toadd$sp == "ACSA", "ACSH", toadd$sp)
#Create unique code column using lat, lon, and sp code for each entry
toadd$ID <- paste0(toadd$Latitude, toadd$Longitude, toadd$sp)

#Read in sp code csv to turn latin names in chrono table to sp code
sp_codes <- read_csv("C:/Users/world/Desktop/sp_codes.csv")
sp_codes <- sp_codes[,c(1,2)]
names(sp_codes) <- c("sp", "Species")

#read in chronology table csv which we are appending the lm results to
chronology_table <- read.csv("doc/manuscript/tables_figures/chronology_table.csv")
chronology_table <- chronology_table[,1:8]
chronology_table$Latitude <- round(chronology_table$Latitude, digits = 7)
chronology_table$Longitude <- round(chronology_table$Longitude, digits = 7)

#Add sp codes to chrono table for unique ID's
chronology_table <- left_join(chronology_table, sp_codes, by = "Species")
chronology_table$ID <- paste0(chronology_table$Latitude, chronology_table$Longitude,chronology_table$sp)

#Left join the two to get LM results appended to chronology table
attempt_3 <- left_join(chronology_table,toadd, by = "ID")
attempt_4 <- attempt_3[!(duplicated(attempt_3$Number)),]#Two sites are duplicated
attempt_4 <- attempt_4[,c(1:8,14:33)]#remove unnecessary columns

#Rename columns to something understandable
names(attempt_4) <- c("Number", "Location",
                      "Species",
                      "Wood Type",
                      "CTW Tmin",
                      "CTW Tmax",
                      "Latitude",
                      "Longitude",
                      "P-value April Tmax",
                      "P-value April - April:June Tmax",
                      "P-value April:June Tmax",
                      "P-value June - April:June Tmax",
                      "P-value April - April:June-July Tmax",
                      "P-value April:June-July Tmax",
                      "P-value June-July - April:June-July Tmax",
                      "P-value April - April:May-August Tmax",
                      "P-value April:May-August Tmax",
                      "P-value May-August - April:May-August Tmax",
                      "Slope April Tmax",
                      "Slope April - April:June Tmax",
                      "Slope April:June Tmax",
                      "Slope June - April:June Tmax",
                      "Slope April - April:June-July Tmax",
                      "Slope April:June-July Tmax",
                      "Slope June-July - April:June-July Tmax",
                      "Slope April - April:May-August Tmax",
                      "Slope April:May-August Tmax",
                      "Slope May-August - April:May-August Tmax")
#Rearrange so slopes are followed by their p-values
#attempt_4 <- attempt_4[,c(1:8,19, 9, 20, 10, 21, 11, 22 , 12, 23, 13, 24, 14, 25, 15, 26, 16, 27, 17, 28, 18)]
attempt_4 <- attempt_4[,c(1:8, 23, 13, 24, 14, 25, 15)]

write.csv(attempt_4, file = "doc/manuscript/tables_figures/chronology_table_2.csv", row.names = FALSE)
