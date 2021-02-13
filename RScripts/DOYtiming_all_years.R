library(ggplot2)
library(readr)
library(tidyverse)
library(lubridate)
Wood_pheno_table_scbi <- read_csv("Data/Wood_pheno_table_SCBI_CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

weatherdata <-
  read_csv("climate data/met_tower_data_sensor2_ncdc_supplemented.csv") %>%
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
    median_windowopendate = as.Date(median_windowopendate),
    median_windowclosedate = as.Date(median_windowclosedate),
    opendoy = yday(median_windowopendate),
    closedoy = yday(median_windowclosedate)
  )

climwinmeans_rp <- weatherdata %>%
  filter(doy %in% c(climwindows[1,11]:climwindows[1,12])) %>%
  group_by(year) %>%
  summarize(climwinmean = mean(cleantmax)) %>%
  mutate(wood_type = "ring-porous")

climwinmeans_dp <- weatherdata %>%
  filter(doy %in% c(climwindows[4,11]:climwindows[4,12])) %>% #68:135
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

aggregates_scbi_rp <- aggregates_scbi_rp[-1,]

aggregates_scbi_rp$Temp <- ifelse(aggregates_scbi_rp$window_temp > median(aggregates_scbi_rp$window_temp), "Above Average",
                                  ifelse(aggregates_scbi_rp$window_temp == median(aggregates_scbi_rp$window_temp), "Median","Below Average"))

aggregates_scbi_dp <- aggregates_scbi_dp[-1,]
aggregates_scbi_dp$Temp <- ifelse(aggregates_scbi_dp$window_temp > median(aggregates_scbi_dp$window_temp), "Above Average",
                                  ifelse(aggregates_scbi_dp$window_temp == median(aggregates_scbi_dp$window_temp), "Median","Below Average"))

aggregates_scbi_rp$stanT <- (aggregates_scbi_rp$window_temp-min(aggregates_scbi_rp$window_temp))/(max(aggregates_scbi_rp$window_temp)-min(aggregates_scbi_rp$window_temp))
aggregates_scbi_dp$stanT <- (aggregates_scbi_dp$window_temp-min(aggregates_scbi_dp$window_temp))/(max(aggregates_scbi_dp$window_temp)-min(aggregates_scbi_dp$window_temp))

aggregates_scbi <- rbind(aggregates_scbi_dp,aggregates_scbi_rp)

#Add leaf Phenology
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

names(leaf_phenology) <- c("site","year","Greenup", "Mid-greenup", "Peak","Senescence","los","tmp")
leaf_phenology_melt <- melt(leaf_phenology, id.vars = c("site","year","tmp","los"))
leaf_phenology_melt <- leaf_phenology_melt[leaf_phenology_melt$year %in% c(2018,2010),]
scbi_leaf <- ggplot(leaf_phenology_melt, aes(x=yday(value), y = as.character(variable), group = year, color = tmp))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Day of Year", y = "Leaf Stage", title = "SCBI Leaf Phenology", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
  scale_colour_gradient(low = "blue", high = "red")
scbi_leaf

ggsave("doc/manuscript/tables_figures/SCBI_leaf.png", plot = scbi_leaf, width = 15, height = 7 / 1.25)

#+ ggplot(aggregates_scbi_dp, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = window_temp, linetype = Group.1))+
#  geom_point(size = 3)+
#  geom_line(size = 1)+
#  theme(legend.position = "top")+
#  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "SCBI Intraannual Growth Timing", color = "Temp", linetype = "Wood Type")+
#  #scale_colour_manual(values = c("red", "blue", "purple"))
#  scale_colour_gradient(low = "blue", high = "red")

#SCBI_CMI <- read_csv("SCBI_CMI.csv")
#CMI_ag_scbi <- aggregate(SCBI_CMI$CMI, by = list(SCBI_CMI$year), FUN = mean)
#names(CMI_ag_scbi) <- c("year","CMI")
#aggregates_scbi <- left_join(aggregates_scbi, CMI_ag_scbi, by = "year")

#CMI_ag_previousyear <- CMI_ag_scbi
#CMI_ag_previousyear[,1] <- c(2012,2013,2014,2015,2016,2017,2018,2019,NA)
#CMI_ag_previousyear <- CMI_ag_previousyear[-9,]

#aggregates_scbi$CMI_level <- ifelse(aggregates_scbi$CMI > mean(aggregates_scbi$CMI), "Above Average", "Below Average")
#aggregates_scbi$previous_cmi <- left_join(aggregates_scbi, CMI_ag_previousyear, by = "year")

#ggplot(aggregates_scbi, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = CMI, linetype = Group.1))+
#  geom_point(size = 3)+
#  geom_line(size = 1)+
#  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "Harvard Forest Intraannual Growth Timing", color = "CMI", linetype = "Wood Type")+
#  scale_colour_gradient(low = "blue", high = "red")


#Harvard Forest
# Get growth data ----------------------------------
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_HarvardForest_CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  #filter(tot >= 1) %>%
  #filter(tot <= 12.06)%>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))
#Wood_pheno_table$tag <- substr(Wood_pheno_table$tag,1, nchar(as.character(Wood_pheno_table$tag))-4)

weatherdata <-
  read_csv("climate data/HF_weatherdata.csv") %>%
  filter(!is.na(airtmax))

#mutate(
#  DATE = dmy(DATE),
#  months = month(DATE, label = TRUE, abbr = FALSE)
#) %>%
# Remove entries with no tmax data
#%>%
# Rename RP flag set by Cam
#rename(flagrp = flag)
climwindows <-
  read.csv("results/Climwin_results/Weekly/Harvard Forest/weekly_climwin_results_HF_TMAX.csv") %>%
  filter(wood_type != "other") %>%
  mutate(
    median_windowopendate = as.Date(median_windowopendate, format = "%Y-%m-%d"),
    median_windowclosedate = as.Date(median_windowclosedate, format = "%Y-%m-%d"),
    opendoy = yday(median_windowopendate),
    closedoy = yday(median_windowclosedate)
  )

climwinmeans_rp_hf <- weatherdata %>%
  filter(DOY %in% c(climwindows[4,11]:climwindows[4,12])) %>%
  group_by(year) %>%
  summarize(climwinmean = mean(airtmax)) %>%
  mutate(wood_type = "ring-porous")

climwinmeans_dp_hf <- weatherdata %>%
  filter(DOY %in% c(climwindows[1,11]:climwindows[1,12])) %>% #68:135
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

aggregates_hf_rp <- aggregates_hf_rp[-1,]
#aggregates_hf_rp$Temp <- ifelse(aggregates_hf_rp$window_temp > median(aggregates_hf_rp$window_temp), "Above Average", "Below Average")
#aggregates_hf_rp[1:3,6] <- "Median"

aggregates_hf_dp <- aggregates_hf_dp[-1,]
#aggregates_hf_dp$Temp <- ifelse(aggregates_hf_dp$window_temp > mean(aggregates_hf_dp$window_temp), "Above Average", "Below Average")
#aggregates_hf_dp[13:15,6] <- "Median"
aggregates_hf_rp$stanT <- (aggregates_hf_rp$window_temp-min(aggregates_hf_rp$window_temp))/(max(aggregates_hf_rp$window_temp)-min(aggregates_hf_rp$window_temp))
aggregates_hf_dp$stanT <- (aggregates_hf_dp$window_temp-min(aggregates_hf_dp$window_temp))/(max(aggregates_hf_dp$window_temp)-min(aggregates_hf_dp$window_temp))

aggregates_hf <- rbind(aggregates_hf_dp,aggregates_hf_rp)

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
hf_leaf_phenology_melt <- hf_leaf_phenology_melt[hf_leaf_phenology_melt$year %in% c(2018,2010),]
hf_leaf <- ggplot(hf_leaf_phenology_melt, aes(x=yday(value), y = as.character(variable), group = year, color = tmp))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Day of Year", y = "Leaf Stage", title = "Harvard Forest Leaf Phenology", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
  scale_colour_gradient(low = "blue", high = "red")
hf_leaf

ggsave("doc/manuscript/tables_figures/HF_leaf.png", plot = hf_leaf, width = 15, height = 7 / 1.25)

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
png(filename = "doc/manuscript/tables_figures/DOYtiming_allyears.png", width=10, height=10,
    pointsize=12, bg="transparent", units="in", res=600,
    restoreConsole=FALSE)

grid.arrange(

  ggplot(aggregates_scbi, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = stanT, linetype = Group.1))+
    geom_point(size = 3)+
    geom_line(size = 1)+
    theme_bw()+
    theme(legend.position = "top")+
    labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "(a) SCBI", color = "Temperature Ratio", linetype = "Wood Type")+
    #scale_colour_manual(values = c("red", "blue", "purple"))
    scale_colour_gradient(low = "blue", high = "red"),

  ggplot(aggregates_hf, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, year), color = stanT, linetype = Group.1))+
    geom_point(size = 3)+
    geom_line(size = 1)+
    theme_bw()+
    theme(legend.position = "none")+
    labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "(b) Harvard Forest", color = "Temp", linetype = "")+
    scale_colour_gradient(low = "blue", high = "red"),

  ggplot(leaf_phenology_melt, aes(x=yday(value), y = as.character(variable), group = year, color = tmp))+
    geom_point(size = 3)+
    geom_line(size = 1)+
    theme(legend.position = "none")+
    labs(x = "Day of Year", y = "Leaf Stage", title = "(c) SCBI Leaf Phenology", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
    scale_colour_gradient(low = "blue", high = "red"),

  ggplot(hf_leaf_phenology_melt, aes(x=yday(value), y = as.character(variable), group = year, color = tmp))+
    geom_point(size = 3)+
    geom_line(size = 1)+
    theme(legend.position = "none")+
    labs(x = "Day of Year", y = "Leaf Stage", title = "(d) Harvard Forest Leaf Phenology", color = "Temp Ratio (1= Warmest pre-season)", linetype = "")+
    scale_colour_gradient(low = "blue", high = "red"),


  as.table = TRUE, nrow=2, ncol=2) ###as.table specifies order if multiple rows

dev.off()
