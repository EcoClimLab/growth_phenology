#Script to analyze the joint effect of spring T and summer drought on tree growth using tree cores#
#Written by: Cameron Dow###########################################################################
#Last edit: 6/30/2021##############################################################################
library(tidyverse)
library(readr)
library(lme4)
library(lmerTest)
library(emmeans)
library(reshape2)
crns <- read_csv("Growth_phenology/Data/tree_rings/Other/all_crns_res_1901.csv") %>%
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
# Load climate data ####

## needs to be in different format: column for Date, year and then one column per climate variable
climate_variables <- c("tmn", "tmx")
clim_v <- NULL
# something like this should do
for(clim_v in climate_variables) {
  print(clim_v)
  x <- read.csv(paste0("Growth_phenology/climate data/CRU/", clim_v,  ".1901.2019-Other_sites-3-11.csv"))


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

melt_cur <- cbind(melt_cur, est)
p_vals <- rbind(p_vals, melt_cur)
}

write.csv(p_vals, file = "Growth_phenology/Data/Tree_core_drought.csv", row.names = FALSE)
