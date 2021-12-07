#Script to create DOY_timing_allyears.png fig

library(ggplot2)
library(readr)
library(tidyverse)
library(lubridate)
library(patchwork)
Wood_pheno_table_scbi <- read_csv("Data/dendrobands/SCBI/modeled/Wood_pheno_table_SCBI_CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

weatherdata <-
  read_csv("climate data/SCBI/met_tower_data_sensor2_ncdc_supplemented.csv") %>%
  filter(!is.na(cleantmax)) %>%
  mutate(year = year.x)

#mutate(
#  DATE = dmy(DATE),
#  months = month(DATE, label = TRUE, abbr = FALSE)
#) %>%
# Remove entries with no tmax data
#%>%
# Rename RP flag set by Cam
#rename(flagrp = flag)
# climwindows <-
#   read.csv("results/Climwin_results/Weekly/SCBI/weekly_climwin_results_SCBI_TMAX.csv") %>%
#   filter(wood_type != "other") %>%
#   mutate(
#     winopen = as.Date(paste(refwoy-winopenwoy, 1, sep="-"), "%U-%u"),
#     winclose = as.Date(paste(refwoy-winclosewoy, 1, sep="-"), "%U-%u"),
#     opendoy = yday(winopen),
#     closedoy = yday(winclose)
#   )

#Method for identifying correct windows is in Pheno_tsensitivity_figure_tmax.R
climwinmeans_rp <- weatherdata %>%
  filter(doy %in% c(92:98)) %>% #April 2 - April 8
  group_by(year) %>%
  summarize(climwinmean = mean(cleantmax)) %>%
  mutate(wood_type = "ring-porous")

climwinmeans_dp <- weatherdata %>%
  filter(doy %in% c(78:140)) %>% # March 19 - May 20
  group_by(year) %>%
  summarize(climwinmean = mean(cleantmax)) %>%
  mutate(wood_type = "diffuse-porous")

# Combine
climwinmeans <- bind_rows(climwinmeans_rp, climwinmeans_dp)

# 3. Add to growth data
Wood_pheno_table_scbi <- Wood_pheno_table_scbi %>%
  #left_join(marchmeans, by = "year") %>%
  left_join(climwinmeans, by = c("year", "wood_type")) %>%
  #left_join(twosevenfive, by = c("tag", "year"))%>%
  #left_join(fiftyseventy, by = c("tag", "year")) %>%
  #left_join(twofifty, by = c("tag", "year")) %>%
  # Remove other variables
  #select(-c(tot, dbh, max_rate_DOY, max_rate)) %>%
  #mutate(
  #  perc = case_when(
  #    perc == 0.25 ~ "DOY_25",
  #    perc == 0.5 ~ "DOY_50",
  #    perc == 0.75 ~ "DOY_75"
  #  )
  #) %>%
  arrange(tag, year)
#View(Wood_pheno_table)

aggregates_scbi_rp <- data.frame(1,1,1,1,1)
names(aggregates_scbi_rp) <- c("Group.1", "Group.2", "x","year", "window_temp")
for(i in 2011:2020){
year <- subset(Wood_pheno_table_scbi, year == i & wood_type == "ring-porous")
year_ag <- aggregate(year$DOY, by = list(year$wood_type, year$perc), FUN = mean)
year_ag$Group.2 <- ifelse(year_ag$Group.2 == .25, 25, ifelse(year_ag$Group.2 == .50, 50, ifelse(year_ag$Group.2 == .75, 75, 0)))
year_ag$year <- i
year_ag$window_temp <- unique(year$climwinmean)
aggregates_scbi_rp <- rbind(aggregates_scbi_rp,year_ag)
}

aggregates_scbi_dp <- data.frame(1,1,1,1,1)
names(aggregates_scbi_dp) <- c("Group.1", "Group.2", "x","year","window_temp")
for(i in 2011:2020){
  year <- subset(Wood_pheno_table_scbi, year == i & wood_type == "diffuse-porous")
  year_ag <- aggregate(year$DOY, by = list(year$wood_type, year$perc), FUN = mean)
  year_ag$Group.2 <- ifelse(year_ag$Group.2 == .25, 25, ifelse(year_ag$Group.2 == .50, 50, ifelse(year_ag$Group.2 == .75, 75, 0)))
  year_ag$year <- i
  year_ag$window_temp <- unique(year$climwinmean)
  aggregates_scbi_dp <- rbind(aggregates_scbi_dp,year_ag)
}

# #MAX RATE DOY ----
# aggregates_scbi_rp <- data.frame(1,1,1,1,1)
# names(aggregates_scbi_rp) <- c("Group.1", "Group.2", "x","year", "window_temp")
# for(i in 2011:2020){
#   year <- subset(Wood_pheno_table_scbi, year == i & wood_type == "ring-porous")
#   year_ag <- aggregate(year$max_rate_DOY, by = list(year$wood_type, year$perc), FUN = mean)
#   year_ag$Group.2 <- ifelse(year_ag$Group.2 == .25, 25, ifelse(year_ag$Group.2 == .50, 50, ifelse(year_ag$Group.2 == .75, 75, 0)))
#   year_ag$year <- i
#   year_ag$window_temp <- unique(year$climwinmean)
#   aggregates_scbi_rp <- rbind(aggregates_scbi_rp,year_ag)
# }
#
# aggregates_scbi_dp <- data.frame(1,1,1,1,1)
# names(aggregates_scbi_dp) <- c("Group.1", "Group.2", "x","year","window_temp")
# for(i in 2011:2020){
#   year <- subset(Wood_pheno_table_scbi, year == i & wood_type == "diffuse-porous")
#   year_ag <- aggregate(year$max_rate_DOY, by = list(year$wood_type, year$perc), FUN = mean)
#   year_ag$Group.2 <- ifelse(year_ag$Group.2 == .25, 25, ifelse(year_ag$Group.2 == .50, 50, ifelse(year_ag$Group.2 == .75, 75, 0)))
#   year_ag$year <- i
#   year_ag$window_temp <- unique(year$climwinmean)
#   aggregates_scbi_dp <- rbind(aggregates_scbi_dp,year_ag)
# }
#----
aggregates_scbi_rp <- aggregates_scbi_rp[-1,]

aggregates_scbi_rp$Temp <- ifelse(aggregates_scbi_rp$window_temp > median(aggregates_scbi_rp$window_temp), "Above Average",
                                  ifelse(aggregates_scbi_rp$window_temp == median(aggregates_scbi_rp$window_temp), "Median","Below Average"))

aggregates_scbi_dp <- aggregates_scbi_dp[-1,]
aggregates_scbi_dp$Temp <- ifelse(aggregates_scbi_dp$window_temp > median(aggregates_scbi_dp$window_temp), "Above Average",
                                  ifelse(aggregates_scbi_dp$window_temp == median(aggregates_scbi_dp$window_temp), "Median","Below Average"))

aggregates_scbi_rp$stanT <- (aggregates_scbi_rp$window_temp-min(aggregates_scbi_rp$window_temp))/(max(aggregates_scbi_rp$window_temp)-min(aggregates_scbi_rp$window_temp))
aggregates_scbi_dp$stanT <- (aggregates_scbi_dp$window_temp-min(aggregates_scbi_dp$window_temp))/(max(aggregates_scbi_dp$window_temp)-min(aggregates_scbi_dp$window_temp))

aggregates_scbi <- rbind(aggregates_scbi_dp,aggregates_scbi_rp)

# #Add slopes of lines for each representing max_rate_DOY change/degree C ----
# doy25_rp <- subset(aggregates_scbi_rp, aggregates_scbi_rp$Group.2 == "25")
# plot(doy25_rp$x~doy25_rp$window_temp)
# abline(lm(doy25_rp$x~doy25_rp$window_temp))
# summary(lm(doy25_rp$x~doy25_rp$window_temp))
#
# #Add slopes of lines for each representing max_rate_DOY change/degree C ----
# doy25_dp <- subset(aggregates_scbi_dp, aggregates_scbi_dp$Group.2 == "25")
# plot(doy25_dp$x~doy25_dp$window_temp)
# abline(lm(doy25_dp$x~doy25_dp$window_temp))
# summary(lm(doy25_dp$x~doy25_dp$window_temp))
#
#Add slopes of lines for each representing DOY change/degree C ----
doy25_rp <- subset(aggregates_scbi_rp, aggregates_scbi_rp$Group.2 == "25")
plot(doy25_rp$x~doy25_rp$window_temp)
abline(lm(doy25_rp$x~doy25_rp$window_temp))
summary(lm(doy25_rp$x~doy25_rp$window_temp))
#DOY25 -1.9128 days/degree C (p = 0.02)
doy25_dp <- subset(aggregates_scbi_dp, aggregates_scbi_dp$Group.2 == "25")
plot(doy25_dp$x~doy25_dp$window_temp)
abline(lm(doy25_dp$x~doy25_dp$window_temp))
summary(lm(doy25_dp$x~doy25_dp$window_temp))
#DOY25 -3.4774 days/degree C (p = 0.002)

doy50_rp <- subset(aggregates_scbi_rp, aggregates_scbi_rp$Group.2 == "50")
plot(doy50_rp$x~doy50_rp$window_temp)
abline(lm(doy50_rp$x~doy50_rp$window_temp))
summary(lm(doy50_rp$x~doy50_rp$window_temp))
#DOY50 -1.5411 days/degree C (p = 0.128)
doy50_dp <- subset(aggregates_scbi_dp, aggregates_scbi_dp$Group.2 == "50")
plot(doy50_dp$x~doy50_dp$window_temp)
abline(lm(doy50_dp$x~doy50_dp$window_temp))
summary(lm(doy50_dp$x~doy50_dp$window_temp))
#DOY50 -3.523 days/degree C (p = 0.01)

doy75_rp <- subset(aggregates_scbi_rp, aggregates_scbi_rp$Group.2 == "75")
plot(doy75_rp$x~doy75_rp$window_temp)
abline(lm(doy75_rp$x~doy75_rp$window_temp))
summary(lm(doy75_rp$x~doy75_rp$window_temp))
#DOY75 -1.128 days/degree C (p = 0.506)
doy75_dp <- subset(aggregates_scbi_dp, aggregates_scbi_dp$Group.2 == "75")
plot(doy75_dp$x~doy75_dp$window_temp)
abline(lm(doy75_dp$x~doy75_dp$window_temp))
summary(lm(doy75_dp$x~doy75_dp$window_temp))
#DOY75 -3.563 days/degree C (p = 0.04)

#Add leaf Phenology----
leaf_phenology <- read_csv("Data/Leaf phenology/leaf_phenology.csv") %>%
  filter(site == "SCBI")
aggregates_scbi <- left_join(aggregates_scbi, leaf_phenology, by = "year")
aggregates_scbi <- aggregates_scbi[complete.cases(aggregates_scbi$los),]
aggregates_scbi$stanlos <- (aggregates_scbi$los-min(aggregates_scbi$los))/(max(aggregates_scbi$los)-min(aggregates_scbi$los))

g <-  ggplot(aggregates_scbi, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = stanT, linetype = Group.1))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme(legend.position = "top")+
  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "SCBI Intraannual Growth Timing", color = "Temp Ratio (1 = Warmest pre-season)", linetype = "Wood Type")+
  #scale_colour_manual(values = c("red", "blue", "purple"))
  scale_colour_gradient(low = "blue", high = "red")
g

#Leaf phenology
leaf_phenology <- read_csv("Data/Leaf phenology/leaf_phenology.csv") %>%
  filter(site == "SCBI")
library(reshape2)
names(leaf_phenology) <- c("site","year","Greenup", "Mid-greenup", "Peak","Senescence","los","tmp")
leaf_phenology_melt <- melt(leaf_phenology, id.vars = c("site","year","tmp","los"))
leaf_phenology_melt$value <- yday(leaf_phenology_melt$value)
aggregate(leaf_phenology_melt$value, by = list(leaf_phenology_melt$variable), FUN = mean)


#Get speed of greenup change / degree C ----
leaf_phenology_melt_GU <- subset(leaf_phenology_melt, leaf_phenology_melt$variable == "Greenup")

plot(leaf_phenology_melt_GU$value~leaf_phenology_melt_GU$tmp)
abline(lm(leaf_phenology_melt_GU$value~leaf_phenology_melt_GU$tmp))
summary(lm(leaf_phenology_melt_GU$value~leaf_phenology_melt_GU$tmp))
#SCBI speed is -4.535 days/degree C

#Get speed of mid-greenup change / degree C ----
leaf_phenology_melt_MG <- subset(leaf_phenology_melt, leaf_phenology_melt$variable == "Mid-greenup")

plot(leaf_phenology_melt_MG$value~leaf_phenology_melt_MG$tmp)
abline(lm(leaf_phenology_melt_MG$value~leaf_phenology_melt_MG$tmp))
summary(lm(leaf_phenology_melt_MG$value~leaf_phenology_melt_MG$tmp))

#Get speed of peak change / degree C ----
leaf_phenology_melt_PK <- subset(leaf_phenology_melt, leaf_phenology_melt$variable == "Peak")

plot(leaf_phenology_melt_PK$value~leaf_phenology_melt_PK$tmp)
abline(lm(leaf_phenology_melt_PK$value~leaf_phenology_melt_PK$tmp))
summary(lm(leaf_phenology_melt_PK$value~leaf_phenology_melt_PK$tmp))

#Get speed of senescence change / degree C ----
leaf_phenology_melt_SC <- subset(leaf_phenology_melt, leaf_phenology_melt$variable == "Senescence")

plot(leaf_phenology_melt_SC$value~leaf_phenology_melt_SC$tmp)
abline(lm(leaf_phenology_melt_SC$value~leaf_phenology_melt_SC$tmp))
summary(lm(leaf_phenology_melt_SC$value~leaf_phenology_melt_SC$tmp))

#Plot leaf variables ----
#leaf_phenology_melt <- leaf_phenology_melt[leaf_phenology_melt$year %in% c(2018,2010),]
scbi_leaf <- ggplot(leaf_phenology_melt, aes(x=value, y = as.character(variable), group = year, color = tmp))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Day of Year", y = "Leaf Stage", title = "SCBI Leaf Phenology", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
  scale_colour_gradient(low = "blue", high = "red")
scbi_leaf

#ggsave("doc/manuscript/tables_figures/SCBI_leaf.png", plot = scbi_leaf, width = 15, height = 7 / 1.25)

#+ ggplot(aggregates_scbi_dp, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = window_temp, linetype = Group.1))+
#  geom_point(size = 3)+
#  geom_line(size = 1)+
#  theme(legend.position = "top")+
#  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "SCBI Intraannual Growth Timing", color = "Temp", linetype = "Wood Type")+
#  #scale_colour_manual(values = c("red", "blue", "purple"))
#  scale_colour_gradient(low = "blue", high = "red")

#precip analysis ----
# SCBI_CMI <- read_csv("climate data/SCBI_CMI.csv")
# SCBI_CMI <- SCBI_CMI[SCBI_CMI$month %in% c("03", "04"),]
# pre_ag_scbi <- aggregate(SCBI_CMI$PRE, by = list(SCBI_CMI$year), FUN = mean)
# names(pre_ag_scbi) <- c("year","PRE")
# aggregates_scbi <- left_join(aggregates_scbi, pre_ag_scbi, by = "year")
#
# pre_ag_previousyear <- pre_ag_scbi
# pre_ag_previousyear[,1] <- c(2012,2013,2014,2015,2016,2017,2018,2019,NA)
# pre_ag_previousyear <- pre_ag_previousyear[-9,]
#
# #aggregates_scbi$PRE_level <- ifelse(aggregates_scbi$CMI > mean(aggregates_scbi$CMI), "Above Average", "Below Average")
# aggregates_scbi <- left_join(aggregates_scbi, pre_ag_previousyear, by = "year")
#
# ggplot(aggregates_scbi, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = PRE.x.x.x, linetype = Group.1))+
#   geom_point(size = 3)+
#   geom_line(size = 1)+
#   labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "Harvard Forest Intraannual Growth Timing", color = "CMI", linetype = "Wood Type")+
#   scale_colour_gradient(low = "blue", high = "red")
#
#
#Harvard Forest
# Get growth data ----------------------------------
Wood_pheno_table <- read_csv("Data/dendrobands/HF/modeled/Wood_pheno_table_HarvardForest_CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  #filter(tot >= 1) %>%
  #filter(tot <= 12.06)%>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))
#Wood_pheno_table$tag <- substr(Wood_pheno_table$tag,1, nchar(as.character(Wood_pheno_table$tag))-4)

weatherdata <-
  read_csv("climate data/HF/HF_weatherdata.csv") %>%
  filter(!is.na(airtmax))

#mutate(
#  DATE = dmy(DATE),
#  months = month(DATE, label = TRUE, abbr = FALSE)
#) %>%
# Remove entries with no tmax data
#%>%
# Rename RP flag set by Cam
#rename(flagrp = flag)
# climwindows <-
#   read.csv("results/Climwin_results/Weekly/Harvard Forest/weekly_climwin_results_HF_TMAX.csv") %>%
#   filter(wood_type != "other") %>%
#   mutate(
#     median_windowopendate = as.Date(median_windowopendate, format = "%Y-%m-%d"),
#     median_windowclosedate = as.Date(median_windowclosedate, format = "%Y-%m-%d"),
#     opendoy = yday(median_windowopendate),
#     closedoy = yday(median_windowclosedate)
#   )

climwinmeans_rp_hf <- weatherdata %>%
  filter(DOY %in% c(85:133)) %>% # March 26 - May 13
  group_by(year) %>%
  summarize(climwinmean = mean(airtmax)) %>%
  mutate(wood_type = "ring-porous")

climwinmeans_dp_hf <- weatherdata %>%
  filter(DOY %in% c(78:133)) %>% # March 19 - May 13
  group_by(year) %>%
  summarize(climwinmean = mean(airtmax)) %>%
  mutate(wood_type = "diffuse-porous")

# Combine
climwinmeans_hf <- bind_rows(climwinmeans_rp_hf, climwinmeans_dp_hf)


# 3. Add to growth data
Wood_pheno_table_hf <- Wood_pheno_table %>%
  #left_join(marchmeans, by = "year") %>%
  left_join(climwinmeans_hf, by = c("year", "wood_type")) %>%
  #left_join(twosevenfive, by = c("tag", "year"))%>%
  #left_join(fiftyseventy, by = c("tag", "year")) %>%
  #left_join(twofifty, by = c("tag", "year")) %>%
  # Remove other variables
  #select(-c(tot, dbh, max_rate_DOY, max_rate)) %>%
  #mutate(
  #  perc = case_when(
  #    perc == 0.25 ~ "DOY_25",
  #    perc == 0.5 ~ "DOY_50",
  #    perc == 0.75 ~ "DOY_75"
  #  )
  #) %>%
  arrange(tag, year)


aggregates_hf_rp <- data.frame(1,1,1,1,1)
names(aggregates_hf_rp) <- c("Group.1", "Group.2", "x","year", "window_temp")
for(i in 1998:2003){
  year <- subset(Wood_pheno_table_hf, year == i & wood_type == "ring-porous")
  year_ag <- aggregate(year$DOY, by = list(year$wood_type, year$perc), FUN = mean)
  year_ag$Group.2 <- ifelse(year_ag$Group.2 == .25, 25, ifelse(year_ag$Group.2 == .50, 50, ifelse(year_ag$Group.2 == .75, 75, 0)))
  year_ag$year <- i
  year_ag$window_temp <- unique(year$climwinmean)
  aggregates_hf_rp <- rbind(aggregates_hf_rp,year_ag)
}

aggregates_hf_dp <- data.frame(1,1,1,1,1)
names(aggregates_hf_dp) <- c("Group.1", "Group.2", "x","year","window_temp")
for(i in 1998:2003){
  year <- subset(Wood_pheno_table_hf, year == i & wood_type == "diffuse-porous")
  year_ag <- aggregate(year$DOY, by = list(year$wood_type, year$perc), FUN = mean)
  year_ag$Group.2 <- ifelse(year_ag$Group.2 == .25, 25, ifelse(year_ag$Group.2 == .50, 50, ifelse(year_ag$Group.2 == .75, 75, 0)))
  year_ag$year <- i
  year_ag$window_temp <- unique(year$climwinmean)
  aggregates_hf_dp <- rbind(aggregates_hf_dp,year_ag)
}

# #DOY GMAX ----
# aggregates_hf_rp <- data.frame(1,1,1,1,1)
# names(aggregates_hf_rp) <- c("Group.1", "Group.2", "x","year", "window_temp")
# for(i in 1998:2003){
#   year <- subset(Wood_pheno_table_hf, year == i & wood_type == "ring-porous")
#   year_ag <- aggregate(year$max_rate_DOY, by = list(year$wood_type, year$perc), FUN = mean)
#   year_ag$Group.2 <- ifelse(year_ag$Group.2 == .25, 25, ifelse(year_ag$Group.2 == .50, 50, ifelse(year_ag$Group.2 == .75, 75, 0)))
#   year_ag$year <- i
#   year_ag$window_temp <- unique(year$climwinmean)
#   aggregates_hf_rp <- rbind(aggregates_hf_rp,year_ag)
# }
#
# aggregates_hf_dp <- data.frame(1,1,1,1,1)
# names(aggregates_hf_dp) <- c("Group.1", "Group.2", "x","year","window_temp")
# for(i in 1998:2003){
#   year <- subset(Wood_pheno_table_hf, year == i & wood_type == "diffuse-porous")
#   year_ag <- aggregate(year$max_rate_DOY, by = list(year$wood_type, year$perc), FUN = mean)
#   year_ag$Group.2 <- ifelse(year_ag$Group.2 == .25, 25, ifelse(year_ag$Group.2 == .50, 50, ifelse(year_ag$Group.2 == .75, 75, 0)))
#   year_ag$year <- i
#   year_ag$window_temp <- unique(year$climwinmean)
#   aggregates_hf_dp <- rbind(aggregates_hf_dp,year_ag)
# }
#
#
#----
aggregates_hf_rp <- aggregates_hf_rp[-1,]
#aggregates_hf_rp$Temp <- ifelse(aggregates_hf_rp$window_temp > median(aggregates_hf_rp$window_temp), "Above Average", "Below Average")
#aggregates_hf_rp[1:3,6] <- "Median"

aggregates_hf_dp <- aggregates_hf_dp[-1,]
#aggregates_hf_dp$Temp <- ifelse(aggregates_hf_dp$window_temp > mean(aggregates_hf_dp$window_temp), "Above Average", "Below Average")
#aggregates_hf_dp[13:15,6] <- "Median"
aggregates_hf_rp$stanT <- (aggregates_hf_rp$window_temp-min(aggregates_hf_rp$window_temp))/(max(aggregates_hf_rp$window_temp)-min(aggregates_hf_rp$window_temp))
aggregates_hf_dp$stanT <- (aggregates_hf_dp$window_temp-min(aggregates_hf_dp$window_temp))/(max(aggregates_hf_dp$window_temp)-min(aggregates_hf_dp$window_temp))

aggregates_hf <- rbind(aggregates_hf_dp,aggregates_hf_rp)

# #Add slopes of lines for each representing max_rate_DOY change/degree C ----
# doy25_rp <- subset(aggregates_hf_rp, aggregates_hf_rp$Group.2 == "25")
# plot(doy25_rp$x~doy25_rp$window_temp)
# abline(lm(doy25_rp$x~doy25_rp$window_temp))
# summary(lm(doy25_rp$x~doy25_rp$window_temp))
#
# #Add slopes of lines for each representing max_rate_DOY change/degree C ----
# doy25_dp <- subset(aggregates_hf_dp, aggregates_hf_dp$Group.2 == "25")
# plot(doy25_dp$x~doy25_dp$window_temp)
# abline(lm(doy25_dp$x~doy25_dp$window_temp))
# summary(lm(doy25_dp$x~doy25_dp$window_temp))

#Add slopes of lines for each representing DOY change/degree C ----
doy25_rp <- subset(aggregates_hf_rp, aggregates_hf_rp$Group.2 == "25")
plot(doy25_rp$x~doy25_rp$window_temp)
abline(lm(doy25_rp$x~doy25_rp$window_temp))
summary(lm(doy25_rp$x~doy25_rp$window_temp))

doy25_dp <- subset(aggregates_hf_dp, aggregates_hf_dp$Group.2 == "25")
plot(doy25_dp$x~doy25_dp$window_temp)
abline(lm(doy25_dp$x~doy25_dp$window_temp))
summary(lm(doy25_dp$x~doy25_dp$window_temp))

doy50_rp <- subset(aggregates_hf_rp, aggregates_hf_rp$Group.2 == "50")
plot(doy50_rp$x~doy50_rp$window_temp)
abline(lm(doy50_rp$x~doy50_rp$window_temp))
summary(lm(doy50_rp$x~doy50_rp$window_temp))

doy50_dp <- subset(aggregates_hf_dp, aggregates_hf_dp$Group.2 == "50")
plot(doy50_dp$x~doy50_dp$window_temp)
abline(lm(doy50_dp$x~doy50_dp$window_temp))
summary(lm(doy50_dp$x~doy50_dp$window_temp))

doy75_rp <- subset(aggregates_hf_rp, aggregates_hf_rp$Group.2 == "75")
plot(doy75_rp$x~doy75_rp$window_temp)
abline(lm(doy75_rp$x~doy75_rp$window_temp))
summary(lm(doy75_rp$x~doy75_rp$window_temp))

doy75_dp <- subset(aggregates_hf_dp, aggregates_hf_dp$Group.2 == "75")
plot(doy75_dp$x~doy75_dp$window_temp)
abline(lm(doy75_dp$x~doy75_dp$window_temp))
summary(lm(doy75_dp$x~doy75_dp$window_temp))


#Add leaf phenology
#leaf_phenology <- read_csv("Data/Leaf phenology/leaf_phenology.csv") %>%
#  filter(site == "HF")
#aggregates_hf <- left_join(aggregates_hf, leaf_phenology, by = "year")
#aggregates_hf <- aggregates_hf[complete.cases(aggregates_hf$los),]
#aggregates_hf$stanlos <- (aggregates_hf$los-min(aggregates_hf$los))/(max(aggregates_hf$los)-min(aggregates_hf$los))

h <- ggplot(aggregates_hf, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = stanT, linetype = Group.1))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme(legend.position = "none")+
  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "Harvard Forest Intraannual Growth Timing", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
  scale_colour_gradient(low = "blue", high = "red")
h

#Leaf phenology plot----
library(reshape2)
leaf_phenology <- read_csv("Data/Leaf phenology/leaf_phenology.csv") %>%
  filter(site == "HF")

leaf_phenology <- leaf_phenology[-3,]
names(leaf_phenology) <- c("site","year","Greenup", "Mid-greenup", "Peak","Senescence","los","tmp")
hf_leaf_phenology_melt <- melt(leaf_phenology, id.vars = c("site","year","tmp","los"))
hf_leaf_phenology_melt$value <- yday(hf_leaf_phenology_melt$value)
aggregate(hf_leaf_phenology_melt$value, by = list(hf_leaf_phenology_melt$variable), FUN = mean)
#hf_leaf_phenology_melt <- hf_leaf_phenology_melt[hf_leaf_phenology_melt$year %in% c(2018,2010),]
hf_leaf <- ggplot(hf_leaf_phenology_melt, aes(x=value, y = as.character(variable), group = year, color = tmp))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Day of Year", y = "Leaf Stage", title = "Harvard Forest Leaf Phenology", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
  scale_colour_gradient(low = "blue", high = "red")
hf_leaf

#ggsave("doc/manuscript/tables_figures/HF_leaf.png", plot = hf_leaf, width = 15, height = 7 / 1.25)

#Get speed of greenup change / degree C ----
hf_leaf_phenology_melt_GU <- subset(hf_leaf_phenology_melt, hf_leaf_phenology_melt$variable == "Greenup")

plot(hf_leaf_phenology_melt_GU$value~hf_leaf_phenology_melt_GU$tmp)
abline(lm(hf_leaf_phenology_melt_GU$value~hf_leaf_phenology_melt_GU$tmp))
summary(lm(hf_leaf_phenology_melt_GU$value~hf_leaf_phenology_melt_GU$tmp))
#HF speed is  -2.437 days/degree C (nonsig)

#Get speed of mid-greenup change / degree C ----
hf_leaf_phenology_melt_MG <- subset(hf_leaf_phenology_melt, hf_leaf_phenology_melt$variable == "Mid-greenup")

plot(hf_leaf_phenology_melt_MG$value~hf_leaf_phenology_melt_MG$tmp)
abline(lm(hf_leaf_phenology_melt_MG$value~hf_leaf_phenology_melt_MG$tmp))
summary(lm(hf_leaf_phenology_melt_MG$value~hf_leaf_phenology_melt_MG$tmp))

#Get speed of peak change / degree C ----
hf_leaf_phenology_melt_PK <- subset(hf_leaf_phenology_melt, hf_leaf_phenology_melt$variable == "Peak")

plot(hf_leaf_phenology_melt_PK$value~hf_leaf_phenology_melt_PK$tmp)
abline(lm(hf_leaf_phenology_melt_PK$value~hf_leaf_phenology_melt_PK$tmp))
summary(lm(hf_leaf_phenology_melt_PK$value~hf_leaf_phenology_melt_PK$tmp))

#Get speed of senescence change / degree C ----
hf_leaf_phenology_melt_SC <- subset(hf_leaf_phenology_melt, hf_leaf_phenology_melt$variable == "Senescence")

plot(hf_leaf_phenology_melt_SC$value~hf_leaf_phenology_melt_SC$tmp)
abline(lm(hf_leaf_phenology_melt_SC$value~hf_leaf_phenology_melt_SC$tmp))
summary(lm(hf_leaf_phenology_melt_SC$value~hf_leaf_phenology_melt_SC$tmp))

# scale_colour_manual(values = c("red", "blue", "purple"))

#h+ ggplot(aggregates_hf_dp, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = window_temp, linetype = Group.1))+
#  geom_point(size = 3)+
#  geom_line(size = 1)+
#  theme(legend.position = "top")+
#  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "Harvard Forest Intraannual Growth Timing", color = "Temp", linetype = "")+
#  scale_colour_gradient(low = "blue", high = "red")
## scale_colour_manual(values = c("red", "blue", "purple"))

#HF_CMI <- read_csv("HF_CMI.csv")
#CMI_ag_hf <- aggregate(HF_CMI$CMI, by = list(HF_CMI$year), FUN = mean)
#names(CMI_ag_hf) <- c("year","CMI")
#aggregates_hf <- left_join(aggregates_hf, CMI_ag_hf, by = "year")

#aggregates_hf$CMI_level <- ifelse(aggregates_hf$CMI > mean(aggregates_hf$CMI), "Above Average", "Below Average")

#CMI_ag_previousyear <- CMI_ag_hf
#CMI_ag_previousyear[,1] <- c(1999,2000,2001,2002,2003,NA)
#CMI_ag_previousyear <- CMI_ag_previousyear[-6,]

#aggregates_hf$previous_cmi <- left_join(aggregates_hf, CMI_ag_previousyear, by = "year")

#ggplot(aggregates_hf, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = CMI, linetype = Group.1))+
#  geom_point(size = 3)+
#  geom_line(size = 1)+
#  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "Harvard Forest Intraannual Growth Timing", color = "CMI", linetype = "Wood Type")+
#  scale_colour_gradient(low = "blue", high = "red")


#SPEI
#spei_all_months <- read_csv("~/GitHub/Climate/Climate_Data/SPEI/data_calculated_with_script/spei_all_months.csv")

#spei_hf <- subset(spei_all_months, sites.sitename == "Harvard_Forest")
#spei_scbi <- subset(spei_all_months, sites.sitename == "Smithsonian_Conservation_Biology_Institute")
#spei_hf$Date <- strptime(spei_hf$Date, format = "%m/%d/%Y")
#spei_hf$Date <- as.Date(spei_hf$Date)

#spei_hf <- spei_hf %>%
#  mutate(year = year(Date),
#                month = month(Date),
#                day = day(Date))
#spei_hf <- subset(spei_hf, year >= 1998 & year <= 2003)
#spei_hf <- spei_hf[,c(1,2,14,51,52,53)]
#spei_agg_hf <- aggregate(spei_hf$value_month_12, by = list(spei_hf$year), FUN = mean)
#aggregates_hf <-

#ggplot(aggregates_hf, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = CMI, linetype = Group.1))+
#  geom_point(size = 3)+
#  geom_line(size = 1)+
#  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "Harvard Forest Intraannual Growth Timing", color = "CMI", linetype = "Wood Type")+
#  scale_colour_gradient(low = "blue", high = "red")

#ALL YEARS PLOT
library(gridExtra)
library(patchwork)
# png(filename = "doc/manuscript/tables_figures/DOYtiming_allyears.png", width=14, height=14,
#     pointsize=12, bg="transparent", units="in", res=600,
#     restoreConsole=FALSE)
#
# grid.arrange(
#
#
#   ggplot(leaf_phenology_melt, aes(x=value, y = as.character(variable), group = year, color = tmp))+
#     geom_point(size = 3)+
#     geom_line(size = 1)+
#     xlim(80,240)+
#     theme_bw()+
#     theme(legend.position = "none",
#           axis.text.y = element_text(angle = 90, hjust = 0.5))+
#     labs(x = "Day of Year", y = "Leaf Stage", title = "(a) SCBI Leaf Phenology", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
#     scale_colour_gradient(low = "blue", high = "red"),
#
#   ggplot(hf_leaf_phenology_melt, aes(x=value, y = as.character(variable), group = year, color = tmp))+
#     geom_point(size = 3)+
#     geom_line(size = 1)+
#     xlim(95,240)+
#     theme_bw()+
#     theme(legend.position = "none",
#           axis.text.y = element_blank())+
#     labs(x = "Day of Year", y = "", title = "(b) Harvard Forest Leaf Phenology", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
#     scale_colour_gradient(low = "blue", high = "red"),
#
#   ggplot(aggregates_scbi, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = stanT, linetype = Group.1))+
#     geom_point(size = 3)+
#     geom_line(size = 1)+
#     theme_bw()+
#     theme(legend.position = c(.87,.5),
#           axis.text.x = element_blank())+
#     xlim(80,240)+
#     labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "(c) SCBI", color = "Temperature Ratio", linetype = "Wood Type")+
#     #scale_colour_manual(values = c("red", "blue", "purple"))
#     scale_colour_gradient(low = "blue", high = "red"),
#
#   ggplot(aggregates_hf, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = stanT, linetype = Group.1))+
#     geom_point(size = 3)+
#     geom_line(size = 1)+
#     theme_bw()+
#     theme(legend.position = "none",
#           axis.text.y = element_blank(),
#           axis.text.x = element_blank())+
#     xlim(95,240)+
#     labs(x = "Day of Year", y = "", title = "(d) Harvard Forest", color = "Temp", linetype = "")+
#     scale_colour_gradient(low = "blue", high = "red"),
#
#   as.table = TRUE, nrow=2, ncol=2) ###as.table specifies order if multiple rows
#
# dev.off()




leaf_phenology_melt$variable <- ifelse(leaf_phenology_melt$variable == "Greenup", "G",
                                       ifelse(leaf_phenology_melt$variable == "Mid-greenup", "M",
                                              ifelse(leaf_phenology_melt$variable == "Peak", "P",
                                                     ifelse(leaf_phenology_melt$variable == "Senescence", "S",NA))))
leaf_phenology_melt$stanT <- (leaf_phenology_melt$tmp-min(leaf_phenology_melt$tmp))/(max(leaf_phenology_melt$tmp)-min(leaf_phenology_melt$tmp))

#Patchwork try
p1 <- ggplot(leaf_phenology_melt, aes(x=value, y = as.character(variable), group = year, color = stanT))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  xlim(80,240)+
  annotate("text", x=85, y=4.35, label= "(a)",size = 10)+
  theme_bw()+
  theme(legend.position = c(.65,.25),
        #axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_blank(),
        text = element_text(size = 20),
        legend.text=element_text(size=14),
        legend.title=element_text(size=13),
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(-1,-1,-1,-1), "cm"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "", y = "Leaf Stage", title = "SCBI", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
  #scale_colour_gradient(low = "blue", high = "red")+
  scale_colour_gradient(low = "blue", high = "red",breaks=c(0,0.5,1),labels=c("0.0 Coldest Year","0.5 Average Year","1.0 Hottest Year"))

p2 <- ggplot(hf_leaf_phenology_melt, aes(x=value, y = as.character(variable), group = year, color = tmp))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  xlim(95,240)+
  annotate("text", x=100, y=4.35, label= "(b)",size = 10)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(-1,-1,-1,-1), "cm"))+
  labs(x = "", y = "", title = "Harvard Forest", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
  scale_colour_gradient(low = "blue", high = "red")

p3 <- ggplot(aggregates_scbi, aes(x=x, y = Group.2, group = interaction(Group.1, year), color = stanT, linetype = Group.1))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme_bw()+
  annotate("text", x=85, y=75, label= "(c)",size = 10)+
  theme(legend.position = c(.75,.3),
        text = element_text(size = 20),
        legend.text=element_text(size=15),
        legend.title=element_text(size=17),
        plot.margin = unit(c(-1,-1,-1,-1), "cm"))+
  guides(shape = guide_legend(override.aes = list(size = 1)),
         color = FALSE) +#guide_legend(override.aes = list(size = 1)))+
  xlim(80,240)+
  ylim(20,80)+
  labs(x = "Day of Year", y = "Stem Growth (% of annual total)", color = "Temperature Ratio", linetype = "Wood Type")+
  #scale_colour_manual(values = c("red", "blue", "purple"))
  scale_colour_gradient(low = "blue", high = "red",breaks=c(0,0.5,1),labels=c("0.0 Coldest Year","0.5 Average Year","1.0 Hottest Year"))

p4 <- ggplot(aggregates_hf, aes(x=x, y = Group.2, group = interaction(Group.1, year), color = stanT, linetype = Group.1))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  annotate("text", x=100, y=75, label= "(d)", size = 10)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        text = element_text(size = 20),
        plot.margin = unit(c(-1,-1,-1,-1), "cm"))+
  xlim(95,240)+
  ylim(20,80)+
  labs(x = "Day of Year", y = "", color = "Temp", linetype = "")+
  scale_colour_gradient(low = "blue", high = "red")

plotall <- p1 + p2 + p3 + p4 +
  plot_layout(heights = c(1.5,2))

png(filename = "doc/manuscript/tables_figures/DOYtiming_allyears.png", width=12, height=10,
    pointsize=12, bg="transparent", units="in", res=600,
    restoreConsole=FALSE)
plotall
dev.off()

#Alan asked if we could calculate heat sum for greenup and other variables to compare ----
Wood_pheno_table_scbi <- read_csv("Data/Wood_pheno_table_SCBI_CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

weatherdata <-
  read_csv("climate data/SCBI/met_tower_data_sensor2_ncdc_supplemented.csv") %>%
  filter(!is.na(cleantmax)) %>%
  mutate(year = year.x)

#mutate(
#  DATE = dmy(DATE),
#  months = month(DATE, label = TRUE, abbr = FALSE)
#) %>%
# Remove entries with no tmax data
#%>%
# Rename RP flag set by Cam
#rename(flagrp = flag)
climwindows <-
  read.csv("results/Climwin_results/Weekly/SCBI/weekly_climwin_results_SCBI_TMAX.csv") %>%
  filter(wood_type != "other") %>%
  mutate(
    winopen = as.Date(paste(refwoy-winopenwoy, 1, sep="-"), "%U-%u"),
    winclose = as.Date(paste(refwoy-winclosewoy, 1, sep="-"), "%U-%u"),
    opendoy = yday(winopen),
    closedoy = yday(winclose)
  )

climwinmeans_rp <- weatherdata %>%
  filter(doy %in% c(92:98)) %>% #April 2 - April 8
  group_by(year) %>%
  summarize(climwinmean = mean(cleantmax)) %>%
  mutate(wood_type = "ring-porous")

climwinmeans_dp <- weatherdata %>%
  filter(doy %in% c(78:140)) %>% # March 19 - May 20
  group_by(year) %>%
  summarize(climwinmean = mean(cleantmax)) %>%
  mutate(wood_type = "diffuse-porous")

# Combine
climwinmeans <- bind_rows(climwinmeans_rp, climwinmeans_dp)

# 3. Add to growth data
Wood_pheno_table_scbi <- Wood_pheno_table_scbi %>%
  #left_join(marchmeans, by = "year") %>%
  left_join(climwinmeans, by = c("year", "wood_type")) %>%
  #left_join(twosevenfive, by = c("tag", "year"))%>%
  #left_join(fiftyseventy, by = c("tag", "year")) %>%
  #left_join(twofifty, by = c("tag", "year")) %>%
  # Remove other variables
  #select(-c(tot, dbh, max_rate_DOY, max_rate)) %>%
  #mutate(
  #  perc = case_when(
  #    perc == 0.25 ~ "DOY_25",
  #    perc == 0.5 ~ "DOY_50",
  #    perc == 0.75 ~ "DOY_75"
  #  )
#) %>%
arrange(tag, year)

wood_pheno_doy <- aggregate(Wood_pheno_table_scbi$DOY, by = list(Wood_pheno_table_scbi$perc, Wood_pheno_table_scbi$year, Wood_pheno_table_scbi$wood_type, Wood_pheno_table_scbi$climwinmean), FUN = mean)
names(wood_pheno_doy) <- c("perc", "year", "wood_type","climwin_mean",  "doy")

weatherdata <-
  read_csv("climate data/SCBI/met_tower_data_sensor2_ncdc_supplemented.csv") %>%
  filter(!is.na(cleantmax)) %>%
  mutate(year = year.x)

#Wood pheno variables ----
wood_pheno_doy$tmax_sum <- NA
for(i in 1:nrow(wood_pheno_doy)){
  #skip_to_next <- FALSE

  weatherdata_sub <- weatherdata[weatherdata$year %in% wood_pheno_doy[i,2],]
  #tryCatch(#trycatch to eliminate errors where no leaf data is present while wood data is
    weatherdata_sub2 <- weatherdata_sub[weatherdata_sub$doy %in% 1:wood_pheno_doy[i,5],]
   # error = function(b) {
    #  skip_to_next <<- TRUE
    #}
  #)
  wood_pheno_doy[i,6] <- sum(weatherdata_sub2$cleantmax)

  #if (skip_to_next) {
  #  next
  #}
}

wood <- ggplot(wood_pheno_doy, aes(x = climwin_mean, y = tmax_sum, group = interaction(perc, wood_type), color = as.factor(perc), shape = wood_type, linetype = wood_type))+
  geom_point()+
  geom_smooth(se = FALSE)+
  theme(legend.position = "top")+
  labs(x = "Mean Window Tmax", y = "Tmax Sum", title= "Wood Phenology", color = "Leaf Stage")


#Leaf pheno variables ----
#Leaf phenology
leaf_phenology <- read_csv("Data/Leaf phenology/leaf_phenology.csv") %>%
  filter(site == "SCBI")
library(reshape2)
names(leaf_phenology) <- c("site","year","Greenup", "Mid-greenup", "Peak","Senescence","los","tmp")
leaf_phenology_melt <- melt(leaf_phenology, id.vars = c("site","year","tmp","los"))
leaf_phenology_melt$value <- yday(leaf_phenology_melt$value)

scbi <- read_csv("climate data/SCBI/scbi_2001to2018.csv")
scbi <- scbi[,c(3,4)]
scbi <- scbi[complete.cases(scbi$TMAX),]
scbi$month <- month(scbi$DATE)
scbi$year <- year(scbi$DATE)
scbi$doy <- yday(scbi$DATE)

# scbi_combo <- left_join(wood_pheno_doy, leaf_phenology_melt[,c(2,5,6)], by = "year")
# scbi_combo$tmax_sum <- NA

leaf_phenology_melt$tmax_sum <- NA
for(i in 1:nrow(leaf_phenology_melt)){
 # skip_to_next <- FALSE

weatherdata_sub <- scbi[scbi$year %in% leaf_phenology_melt[i,2],]
#tryCatch(#trycatch to eliminate errors where no leaf data is present while wood data is
weatherdata_sub2 <- weatherdata_sub[weatherdata_sub$doy %in% 1:leaf_phenology_melt[i,6],]
#error = function(b) {
#  skip_to_next <<- TRUE
#}
#)
leaf_phenology_melt[i,7] <- sum(weatherdata_sub2$TMAX)

#if (skip_to_next) {
#  next
#}
}

leaf <- ggplot(leaf_phenology_melt, aes(x = tmp, y = tmax_sum, group = variable, color = variable))+
  geom_point()+
  geom_smooth(se = FALSE)+
  theme(legend.position = "top")+
  labs(x = "Mean Window Tmax", y = "Tmax Sum", title= "Leaf Phenology", color = "Leaf Stage")

both <- wood + leaf +
  plot_layout(ncol = 2,nrow = 1, heights = c(1,1))

png(filename = "doc/manuscript/tables_figures/tmax_sums.png", width=15, height=10,
    pointsize=12, bg="transparent", units="in", res=600,
    restoreConsole=FALSE)
both
dev.off()
