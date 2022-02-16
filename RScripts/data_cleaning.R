#Setup ------------------------------------------------------------------------
#
# This script takes as inputs the SCBI and Harvard Forest RAW
#
# 1. wood phenology DOY25, DOY50, DOY75 data
# 2. fitted parameters and related values (max rate, max rate DOY, etc.)
#
# and removes "outlier" tree-years based on whether their fitted growth curves
# make sense. These CLEAN data sets are then output for use in our main analysis
# script Pheno_tsensitivity_figure.R
#
# To get a quick overview of the sections of this code, go to RStudio menu bar ->
# Edit -> Folding -> Collapse all.
#
library(tidyverse)
library(scales)
library(tidybayes)

# Number of standard deviations used to identify "outliers"
sd <- 2

# Load Sean McMahon's Dendroband functions
source("RScripts/dendroband_functions.R")


# SCBI -------------------------------------------------------------------------
## Load up percent growth DF ----
Wood_pheno_table_scbi <- read_csv("Data/dendrobands/SCBI/modeled/Wood_pheno_table_SCBI_RAW.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))
Wood_pheno_table_scbi$tag_year_perc <- paste0(Wood_pheno_table_scbi$tag, Wood_pheno_table_scbi$year, Wood_pheno_table_scbi$perc)
unitag <- unique(Wood_pheno_table_scbi)

LG5_parameter_values_scbi <- read_csv("Data/dendrobands/SCBI/modeled/LG5_parameter_values_SCBI_RAW.csv")

percent_growth <- Wood_pheno_table_scbi %>%
  separate(tag, into = c("tag", "stem"), sep = "_") %>%
  mutate(
    tag = as.numeric(tag),
    tag_year = str_c(tag, year)
  ) %>%
  select(tag, year, tag_year, wood_type) %>%
  distinct() %>%
  left_join(LG5_parameter_values_scbi, by = c("tag", "year")) %>% ###
  group_by(tag, year) %>%
  mutate(doy = list(seq(from = -1, to = 365, by = 1))) %>%
  unnest_longer(doy) %>%
  group_by(tag_year) %>%
  mutate(
    dbh = lg5(L, K, doy_ip, r, theta, doy),
    #dbh_other = lg5.pred(L, K, doy_ip, r, theta, doy),
    dbh_growth = dbh - lag(dbh),
    dbh_total_growth = b - a,
    dbh_growth_percent = dbh_growth / (max(dbh) - min(dbh))
  ) %>%
  filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )
#
# percs_new <- data.frame(NULL)
# for(i in 1:length(unique(percent_growth$tag_year))){
#   try_df <-percent_growth[percent_growth$tag_year %in%
#                           unique(percent_growth$tag_year)[i],]
#   try_df$perc <- seq(0,365,1)
#
# for (p in c(.25, .50, .75)) {
#   try <- try_df$doy[which(abs(try_df$dbh - (try_df$a + try_df$dbh_total_growth*p)) == min(abs(try_df$dbh - (try_df$a + try_df$dbh_total_growth*p))))]
#   try_df$perc[try] <- p # assign the number to the value found above
# percs_new <- rbind(percs_new, try_df)
#   }
# }
#
# percs_new_sub <- percs_new[percs_new$perc %in% c(0.25,0.50,0.75),]
# percs_new_sub$tag_year_perc <- paste0(percs_new_sub$tag,"_1",percs_new_sub$year, percs_new_sub$perc)
# percs_new_sub <- percs_new_sub[!(duplicated(percs_new_sub$tag_year_perc)),]
# percs_new_sub <- percs_new_sub[,c(22,13)]
#
# Wood_pheno_table_scbi <- left_join(Wood_pheno_table_scbi, percs_new_sub, by = "tag_year_perc")
#
#
# # percent_growth$min_check <- ifelse(percent_growth$a == percent_growth$dbh_min, 1 , 0)
# # percent_growth$max_check <- ifelse(percent_growth$b == percent_growth$dbh_max, 1 , 0)
# #
# # percent_growth$min_check <- ifelse(percent_growth$L == percent_growth$dbh_min, 1 , 0)
# # percent_growth$max_check <- ifelse(percent_growth$K == percent_growth$dbh_max, 1 , 0)
# #
# # yes_min <- percent_growth[percent_growth$min_check %in% 1,]
# # unique(yes_min$tag_year)
# #
# # yes_max <- percent_growth[percent_growth$max_check %in% 1,]
# # unique(yes_max$tag_year)
#
#
#
# Wood_pheno_table_scbi <- Wood_pheno_table_scbi %>%
#   separate(tag, into = c("tag", "stem"), sep = "_") %>%
#   mutate(
#     tag = as.numeric(tag),
#     tag_year = str_c(tag, year)
#   )
#
# Wood_pheno_table_scbi <- Wood_pheno_table_scbi[!is.na(Wood_pheno_table_scbi$doy),]
# write.csv(Wood_pheno_table_scbi, file = "Data/dendrobands/SCBI/modeled/wood_table_scbi_updated.csv")

# # #Remove tag years where doy% variable is > 3 SD away from mean

Wood_pheno_table_scbi <- read_csv("Data/dendrobands/SCBI/modeled/wood_table_scbi_updated.csv")
Wood_pheno_table_scbi <- Wood_pheno_table_scbi[,-1]

means <- aggregate(Wood_pheno_table_scbi$doy, by = list(Wood_pheno_table_scbi$perc,Wood_pheno_table_scbi$year, Wood_pheno_table_scbi$wood_type), FUN = mean)
SD <- aggregate(Wood_pheno_table_scbi$doy, by = list(Wood_pheno_table_scbi$perc,Wood_pheno_table_scbi$year, Wood_pheno_table_scbi$wood_type), FUN = sd)
names(SD) <- c("Group.1", "Group.2","Group.3", "sd")
means <- left_join(means,SD)
names(means) <- c("perc","year", "wood_type", "mean", "SD")

Wood_pheno_table_scbi <- left_join(Wood_pheno_table_scbi, means, by = c("perc","year", "wood_type"))

Wood_pheno_table_scbi$rm <- ifelse(Wood_pheno_table_scbi$doy > Wood_pheno_table_scbi$mean+(3*Wood_pheno_table_scbi$SD) | Wood_pheno_table_scbi$doy < Wood_pheno_table_scbi$mean-(3*Wood_pheno_table_scbi$SD), "rm", "keep")
bad_doys <- as.character(unique(Wood_pheno_table_scbi[Wood_pheno_table_scbi$rm %in% "rm",]$tag_year)) #27
bad_doy_tags <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% bad_doys,]

Wood_pheno_table_scbi <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$rm %in% "keep",]
perc_check <- aggregate(Wood_pheno_table_scbi$perc, by = list(Wood_pheno_table_scbi$tag_year), FUN = sum)
perc_check <- perc_check[perc_check$x == 1.5,]
not_in <- Wood_pheno_table_scbi[!(Wood_pheno_table_scbi$tag_year %in% perc_check$Group.1),] #2970 -> 2871 3135 -> 2838 3135 -> 3054

Wood_pheno_table_scbi <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% perc_check$Group.1,] #2970 -> 2871 3135 -> 2838 3135 -> 3054

percent_growth <- percent_growth[percent_growth$tag_year %in% Wood_pheno_table_scbi$tag_year,]
Wood_pheno_table_scbi <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% Wood_pheno_table_scbi$tag_year,]

# #Remove years where raw measures past doy 250 were >20% of total growth
# all_stems <- read_csv("Data/dendrobands/SCBI/raw_data/all_stems.csv")
# all_stems$tag_year <- paste0(all_stems$tag, all_stems$year)
# min <- aggregate(all_stems$dbh2, by = list(all_stems$tag_year), FUN = min)
# names(min) <- c("tag_year","min")
# max <- aggregate(all_stems$dbh2, by = list(all_stems$tag_year), FUN = max)
# names(max) <- c("tag_year","max")
# raw_total <- left_join(min, max)
# raw_total$tot <- raw_total$max-raw_total$min
# raw_total <- raw_total[,c(-2,-3)]
# all_stems <- left_join(all_stems, raw_total, by = "tag_year")
#
# new_stems <- data.frame(NULL)
#
# tag_years <- unique(all_stems$tag_year)
#
# for(i in 1:length(tag_years)){
#
# sub_stems <- all_stems[all_stems$tag_year %in% tag_years[i],]
# sub_stems$growth <-  sub_stems$dbh2 - lag(sub_stems$dbh2)
# new_stems <- rbind(new_stems, sub_stems)
#
# }
# late_growth <- new_stems[new_stems$DOY >= 250,]
# late_growth$perc_growth <- late_growth$growth/late_growth$tot
# late_growth_rm <- late_growth[late_growth$perc_growth > 0.30,]
#
# percent_growth <- percent_growth[!(percent_growth$tag_year %in% late_growth_rm$tag_year),]
# Wood_pheno_table_scbi <- Wood_pheno_table_scbi[!(Wood_pheno_table_scbi$tag_year %in% late_growth_rm$tag_year),] #2871 -> 2706
######################################################

#Remove years where first measurement occured after latest spring survey - 4/18
all_stems <- read_csv("Data/dendrobands/SCBI/raw_data/all_stems.csv")
all_stems$tag_year <- paste0(all_stems$tag, all_stems$year)

#ID where latest spring survey occured
all_stems$survey_num <- substr(all_stems$survey.ID, nchar(all_stems$survey.ID)-2,nchar(all_stems$survey.ID))
first <- all_stems[all_stems$survey_num %in% ".01",]

#remove stem years where first survey was missed
library(lubridate)
latest_day <- yday(as.Date("4/18/2017", format = "%m/%d/%Y"))

start_days <- aggregate(all_stems$DOY, by = list(all_stems$tag_year), FUN = min)
start_days <- start_days[start_days$x > latest_day,]

#Look at the tree years
late_starts <- all_stems[all_stems$tag_year %in% start_days$Group.1,]

#remove from percent growth and wood_pheno
percent_growth <- percent_growth[!(percent_growth$tag_year %in% late_starts$tag_year),]
late_survey <- Wood_pheno_table_scbi[(Wood_pheno_table_scbi$tag_year %in% late_starts$tag_year),]

Wood_pheno_table_scbi <- Wood_pheno_table_scbi[!(Wood_pheno_table_scbi$tag_year %in% late_starts$tag_year),] # 3054 -> 2994

#Subset percent growth to only contain modeled growth in b-a
percent_growth <- subset(percent_growth, percent_growth$dbh > percent_growth$a & percent_growth$dbh < percent_growth$b)

## Clean the data ----
# Break into RP and DP
percent_growth_RP <- subset(percent_growth, wood_type == "ring-porous")

# first_days <- aggregate(percent_growth_RP$doy, by = list(percent_growth_RP$tag_year), FUN= min)
# names(first_days) <- c("tag_year", "start")
# first_days$tag_year <- as.character(first_days$tag_year)
# mean(first_days$start) + (2*sd(first_days$start))
# plot(first_days$start)
#
# twentyfive <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$perc %in% 0.25,]
# twentyfive <- twentyfive[,c(14,13)]
# names(twentyfive) <- c("tag_year", "25")
# twentyfive$tag_year <- as.character(twentyfive$tag_year)
#
# fifty <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$perc %in% 0.50,]
# fifty <- fifty[,c(14,13)]
# names(fifty) <- c("tag_year", "50")
# fifty$tag_year <- as.character(fifty$tag_year)
#
# seventyfive <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$perc %in% 0.75,]
# seventyfive <- seventyfive[,c(14,13)]
# names(seventyfive) <- c("tag_year", "75")
# seventyfive$tag_year <- as.character(seventyfive$tag_year)
#
# first_days <- left_join(first_days, twentyfive, by = "tag_year")
# first_days <- left_join(first_days, fifty, by = "tag_year")
# first_days <- left_join(first_days, seventyfive, by = "tag_year")
#
# library(reshape2)
# first_days <- melt(first_days, id.vars = "tag_year")
# SCBI_RP <- ggplot(first_days, aes( x = tag_year, y = value, color = variable)) +
#   geom_point()

# Remove models with single day growth greater than 2 SD away from mean

maxgrowthrate <- aggregate(percent_growth_RP$dbh_growth_percent, by = list(percent_growth_RP$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate") # Maximum growth rate of each tree
# high_growthday <- subset(maxgrowthrate, rate >= 0.02) #Remove trees where one day had higher than 2% of total growth
high_growthday <- subset(maxgrowthrate, rate >= (mean(maxgrowthrate$rate + (sd(maxgrowthrate$rate) * sd))) | rate <= 0) # Remove trees where one day had higher than 4% of total growth

# ENTRIES REMOVED IN THIS STEP
percent_growth_high <- percent_growth_RP[percent_growth_RP$tag_year %in% high_growthday$tag_year, ]
SCBI_highgrowth_RP <- unique(percent_growth_high$tag_year)# rm 11 - all RP
wood_pheno_removed <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% SCBI_highgrowth_RP, ]
wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
count(wood_pheno_removed, sp)
hist(wood_pheno_removed$dbh)
# subset main DF
percent_growth_RP <- percent_growth_RP[!(percent_growth_RP$tag_year %in% unique(high_growthday$tag_year)), ]

# Remove models with peak growth outside of expected season. IE, winter growth peaks
ratedoy <- percent_growth_RP[percent_growth_RP$dbh_growth_percent %in% maxgrowthrate$rate, ] # doy where max growth occured
keeptags <- NULL
for (i in 2011:2020) {
  year <- subset(ratedoy, year == i)
  upper <- (mean(year$doy) + (sd * sd(year$doy)))
  lower <- (mean(year$doy) - (sd * sd(year$doy)))
  year <- subset(year, doy <= upper & doy >= lower) # Remove models where peak growth was in winter
  # year <- year[year$tag_year %in% ratedoysubset$tag_year,]
  goodtags <- unique(year$tag_year)
  keeptags <- append(keeptags, goodtags)
}

percent_growth_RP_gone <- percent_growth_RP[!(percent_growth_RP$tag_year %in% keeptags),]
SCBI_badpeak_RP <- unique(percent_growth_RP_gone$tag_year) # rm 29

percent_growth_RP <- percent_growth_RP[percent_growth_RP$tag_year %in% keeptags,]
#ENTRIES REMOVED IN THIS STEP
#percent_growth_badpeak <- percent_growth_RP[!(percent_growth_RP$tag_year %in% ratedoysubset$tag_year),]
#SCBI_badpeak_RP <- unique(percent_growth_badpeak$tag_year)
#wood_pheno_removed <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% SCBI_badpeak_RP,]
#wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
#count(wood_pheno_removed,sp)
#hist(wood_pheno_removed$dbh)

#subset main DF
#percent_growth_RP <- percent_growth_RP[percent_growth_RP$tag_year %in% ratedoysubset$tag_year,]

#DIffuse porous - Same method
percent_growth_DP <- subset(percent_growth, wood_type == "diffuse-porous")
#
# first_days <- aggregate(percent_growth_DP$doy, by = list(percent_growth_DP$tag_year), FUN= min)
# names(first_days) <- c("tag_year", "start")
# first_days$tag_year <- as.character(first_days$tag_year)
# mean(first_days$start) + (2*sd(first_days$start))
# plot(first_days$start)
#
# twentyfive <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$perc %in% 0.25,]
# twentyfive <- twentyfive[,c(14,13)]
# names(twentyfive) <- c("tag_year", "25")
# twentyfive$tag_year <- as.character(twentyfive$tag_year)
#
# fifty <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$perc %in% 0.50,]
# fifty <- fifty[,c(14,13)]
# names(fifty) <- c("tag_year", "50")
# fifty$tag_year <- as.character(fifty$tag_year)
#
# seventyfive <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$perc %in% 0.75,]
# seventyfive <- seventyfive[,c(14,13)]
# names(seventyfive) <- c("tag_year", "75")
# seventyfive$tag_year <- as.character(seventyfive$tag_year)
#
# first_days <- left_join(first_days, twentyfive, by = "tag_year")
# first_days <- left_join(first_days, fifty, by = "tag_year")
# first_days <- left_join(first_days, seventyfive, by = "tag_year")
#
# library(reshape2)
# first_days <- melt(first_days, id.vars = "tag_year")
# SCBI_DP <- ggplot(first_days, aes( x = tag_year, y = value, color = variable)) +
#   geom_point()

# Remove models with single day growth rate >2 SD away from mean
maxgrowthrate <- aggregate(percent_growth_DP$dbh_growth_percent, by = list(percent_growth_DP$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate")#Maximum growth rate of each tree
high_growthday <- subset(maxgrowthrate, rate >= (mean(maxgrowthrate$rate)+(sd(maxgrowthrate$rate)*sd)) & rate <= 0)
#ENTRIES REMOVED IN THIS STEP
percent_growth_high <- percent_growth_DP[percent_growth_DP$tag_year %in% high_growthday$tag_year,]
SCBI_highgrowth_DP <- unique(percent_growth_high$tag_year) # rm 0
#wood_pheno_removed <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% SCBI_highgrowth_DP,]
#wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
#count(wood_pheno_removed,sp)
#hist(wood_pheno_removed$dbh)

#Subset main DF
percent_growth_DP <- percent_growth_DP[!(percent_growth_DP$tag_year %in% unique(high_growthday$tag_year)),]

#Remove models with peak growth outside of expected season. IE, winter growth peaks
ratedoy <- percent_growth_DP[percent_growth_DP$dbh_growth_percent %in% maxgrowthrate$rate,]#doy where max growth occured

keeptags <- NULL
for (i in 2011:2020) {
  year <- subset(ratedoy, year == i)
  upper <- (mean(year$doy) + (sd * sd(year$doy)))
  lower <- (mean(year$doy) - (sd * sd(year$doy)))
  year <- subset(year, doy <= upper & doy >= lower) # Remove models where peak growth was in winter
  # year <- year[year$tag_year %in% ratedoysubset$tag_year,]
  goodtags <- unique(year$tag_year)
  keeptags <- append(keeptags, goodtags)
}

percent_growth_DP_gone <- percent_growth_DP[!(percent_growth_DP$tag_year %in% keeptags),]
SCBI_badpeak_DP <- unique(percent_growth_DP_gone$tag_year) # rm 15

percent_growth_DP <- percent_growth_DP[percent_growth_DP$tag_year %in% keeptags,]
#ENTRIES REMOVED IN THIS STEP
#percent_growth_badpeak <- percent_growth_DP[!(percent_growth_DP$tag_year %in% ratedoysubset$tag_year),]
#SCBI_badpeak_DP <- unique(percent_growth_badpeak$tag_year)
#wood_pheno_removed <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% SCBI_badpeak_DP,]
#wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
#count(wood_pheno_removed,sp)
#hist(wood_pheno_removed$dbh)

#subset main DF
#percent_growth_DP <- percent_growth_DP[percent_growth_DP$tag_year %in% ratedoysubset$tag_year,]

#Re-combine into single DF
percent_growth <- rbind(percent_growth_DP, percent_growth_RP)

## Clean both at once now ----
# Remove small/negligable growth models...Seem to be more of a problem with Harvard Forest but remove here for consistency
percent_growth_gone <- subset(percent_growth, dbh_total_growth <= 0.05) # remove small growth trees
SCBI_smallgrowth <- unique(percent_growth_gone$tag_year) # rm 8
percent_growth <- subset(percent_growth, dbh_total_growth >= 0.05) # remove small growth trees
# Remove models that failed to model at least 97.5% of total growth using LG5.pred. Indicates that model fit poorly
#percent_growth_one <- subset(percent_growth, dbh_growth_percent_cummulative >= .975) # Find tags that reach ~100% growth
#unique(percent_growth_one$tag_year) # Check how many tags meet this req
# ENTRIES REMOVED IN THIS STEP
# percent_growth_low <- percent_growth[!(percent_growth$tag_year %in% unique(percent_growth_one$tag_year)), ]
# SCBI_below99 <- unique(percent_growth_low$tag_year)
# wood_pheno_removed <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% SCBI_below99, ]
# wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
# count(wood_pheno_removed, sp)
# ggplot(wood_pheno_removed, aes(x = dbh, group = sp, fill = sp)) +
#   geom_histogram(position = "dodge")
# ggplot(Wood_pheno_table_scbi, aes(x = dbh, group = sp, fill = sp)) +
#   geom_histogram(position = "dodge")
#
# hist(wood_pheno_removed$year)
# LG5_parameter_values_removed_99_scbi <- LG5_parameter_values_scbi[LG5_parameter_values_scbi$tag_year %in% SCBI_below99, ]
# hist(LG5_parameter_values_scbi$ML_value)
# hist(LG5_parameter_values_removed_99_scbi$ML_value)
#
# Subset main DF
#percent_growth <- percent_growth[(percent_growth$tag_year %in% unique(percent_growth_one$tag_year)), ] # Remove tags with <97.5% growth

#Strange spikes
# percent_growth_na <- percent_growth[(is.na(percent_growth$b)),]
# SCBI_na <- unique(percent_growth_na$tag_year)
# percent_growth <- percent_growth[!(is.na(percent_growth$b)),]

## Check for patterns in removed trees ----
removed_tagyears <- c(SCBI_badpeak_DP, SCBI_badpeak_RP, SCBI_highgrowth_DP, SCBI_highgrowth_RP, SCBI_smallgrowth)
removed_trees <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% removed_tagyears, ]
removed_trees <- rbind(removed_trees, bad_doy_tags, late_survey)
removed_trees <- subset(removed_trees, perc == .25)
hist(removed_trees$dbh)
count(removed_trees, sp)

Wood_pheno_table_scbi_25 <- subset(Wood_pheno_table_scbi, perc == .25)
library(gridExtra)
png(filename = "doc/manuscript/tables_figures/not_in_manuscript_or_SI/data_cleaning_figure_scbi.png", width=15, height=10,
    pointsize=12, bg="transparent", units="in", res=600,
    restoreConsole=FALSE)

grid.arrange(

  ggplot(Wood_pheno_table_scbi_25, aes(x = dbh, fill = sp))+geom_histogram(position = "dodge", binwidth = 20)+
    labs(x = "DBH", y = "# of tag years", title = "Tag years pre-removal", fill = "")+
    theme_bw()+
    theme(legend.position = "none"),

  ggplot(removed_trees, aes(x = dbh, fill = sp))+geom_histogram(position = "dodge", binwidth = 20)+
    labs(x = "DBH", y = "", title = "Tag years removed", fill = "Species code")+
    theme_bw()+
    theme(legend.position = c(.9,.60)),

  ggplot(Wood_pheno_table_scbi_25, aes(x = sp, fill = sp))+geom_bar(position = "dodge")+
    labs(x = "Species", y = "# of tag years", title = "", fill = "")+
    theme_bw()+
    theme(legend.position = "none"),

  ggplot(removed_trees, aes(x = sp, fill = sp))+geom_bar(position = "dodge")+
    labs(x = "Species", y = "", title = "", fill = "")+
    theme_bw()+
    theme(legend.position = "none"),

  as.table = TRUE, nrow=2, ncol=2) ###as.table specifies order if multiple rows

dev.off()

#There are two tags that were missed from the previous sections that are visually identified as poor fits/outliers. We remove these
# weirdtag <- subset(percent_growth, doy == 205)
# weirdtag <- subset(weirdtag, dbh_growth_percent == max(weirdtag$dbh_growth_percent))
#
# percent_growth <- percent_growth[!(percent_growth$tag_year %in% unique(weirdtag$tag_year)), ]
#
# weirdtag <- subset(percent_growth, doy == 353:365)
# weirdtag <- subset(weirdtag, dbh_growth_percent == max(weirdtag$dbh_growth_percent))
#
# percent_growth <- percent_growth[!(percent_growth$tag_year %in% unique(weirdtag$tag_year)), ]

## Remove bad models from wood_pheno_table and LG5_param DF's ----
# Wood_pheno_table_scbi <-  Wood_pheno_table_scbi %>%
#  separate(tag, into = c("tag", "stem"), sep = "_") %>%
#  mutate(
#    tag = as.numeric(tag),
#    tag_year = str_c(tag, year)
#  )
#unitag <- unique(Wood_pheno_table_scbi)

wood_gone <- Wood_pheno_table_scbi[!(Wood_pheno_table_scbi$tag_year %in% unique(percent_growth$tag_year)),]
wood_gone<- subset(wood_gone, perc == .25)

Wood_pheno_table_scbi <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% unique(percent_growth$tag_year), ]
tot_growth <- distinct(percent_growth[, c(3, 16)], .keep_all = TRUE)
Wood_pheno_table_scbi$tag_year <- as.character(Wood_pheno_table_scbi$tag_year)
Wood_pheno_table_scbi <- left_join(Wood_pheno_table_scbi, tot_growth, by = "tag_year")
# unitag <- unique(Wood_pheno_table_scbi)


LG5_parameter_values_scbi$tag_year <- paste0(LG5_parameter_values_scbi$tag, LG5_parameter_values_scbi$year)
LG5_parameter_values_scbi <- LG5_parameter_values_scbi[LG5_parameter_values_scbi$tag_year %in% unique(percent_growth$tag_year), ]

# V11 = 99%, V12 = 95%, V13 = 97.5%
Wood_pheno_table_scbi <- Wood_pheno_table_scbi[!duplicated(Wood_pheno_table_scbi$tag_year_perc),]
write.csv(Wood_pheno_table_scbi, file = "data/dendrobands/SCBI/modeled/Wood_pheno_table_SCBI_CLEAN.csv", row.names = FALSE)
write.csv(LG5_parameter_values_scbi, file = "data/dendrobands/SCBI/modeled/LG5_parameter_values_SCBI_CLEAN.csv", row.names = FALSE)
## SCBI Plots ----
rel_growth_scbi <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent, group = tag_year)) +
  coord_cartesian(ylim = c(0, 0.05)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "DOY", y = "Percent of total growth",
    title = "Percent of total (modeled) growth in diameter",
    subtitle = "Each curve represents one tag-year"
  ) +
  theme_bw() +
  geom_line(alpha = 0.2, aes(col = wood_type)) +
  scale_colour_viridis_d("Wood Type", end = 2 / 3)
rel_growth_scbi
# fig_width <- 7
# ggsave(filename = "doc/manuscript/tables_figures/rel_growth.png",
#       plot = rel_growth,
#       width = fig_width, height = fig_width / 2)


#Instead of removing rows prior to a, can you make them NA, then change NA's to 0 here?
fig3_scbi <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent_cummulative, group = tag_year, col = wood_type)) +
  geom_line(alpha = 0.2) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Cummulative percent of total growth", title = "Cummulative percent of total (modeled) growth in diameter")
fig3_scbi

# fig_width <- 6
# ggsave(filename = "doc/manuscript/tables_figures/fig3.png", plot = fig3, width = fig_width, height = fig_width / 1.52)

# Harvard Forest ---------------------------------------------------------------
## Load up percent growth DF ----

Wood_pheno_table_hf <- read_csv("Data/dendrobands/HF/modeled/Wood_pheno_table_HarvardForest_RAW.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other",
         year != 1998) %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

Wood_pheno_table_hf$site_tag <- Wood_pheno_table_hf$tag
Wood_pheno_table_hf$site <- substr(Wood_pheno_table_hf$tag, 1, 2)
Wood_pheno_table_hf$realtag <- substr(Wood_pheno_table_hf$site_tag, 3, nchar(as.character(Wood_pheno_table_hf$site_tag)))

LG5_parameter_values_hf <- read_csv("Data/dendrobands/HF/modeled/LG5_parameter_values_HarvardForest_RAW.csv")
LG5_parameter_values_hf$site_tag <- paste0(LG5_parameter_values_hf$plot, LG5_parameter_values_hf$tag)

percent_growth <- Wood_pheno_table_hf %>%
  # separate(tag, into = c("tag", "stem"), sep = "_") %>%
  mutate(
    site_tag = site_tag,
    tag_year = str_c(site_tag, year)
  ) %>%
  select(site_tag, year, tag_year, wood_type) %>%
  distinct() %>%
  left_join(LG5_parameter_values_hf, by = c("site_tag", "year")) %>%
  group_by(site_tag, year) %>%
  mutate(doy = list(seq(from = 0, to = 365, by = 1))) %>%
  unnest_longer(doy) %>%
  group_by(tag_year) %>%
  mutate(
    dbh = lg5(L, K, doy_ip, r, theta, doy),
    dbh_growth = dbh - lag(dbh),
    dbh_total_growth = b - a,
    dbh_growth_percent = dbh_growth / (max(dbh) - min(dbh))
  ) %>%
  filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )
Wood_pheno_table_hf$tag_year <- paste0(Wood_pheno_table_hf$tag, Wood_pheno_table_hf$year)

LG5_parameter_values_hf$tag_year <- paste0(LG5_parameter_values_hf$tag, LG5_parameter_values_hf$year)

#New DOY variables

# percs_new <- data.frame(NULL)
# for(i in 1:length(unique(percent_growth$tag_year))){
#   try_df <-percent_growth[percent_growth$tag_year %in% unique(percent_growth$tag_year)[i],]
#   try_df$perc <- seq(1,365,1)
#
#   for (p in c(.25, .50, .75)) {
#     try <- try_df$doy[which(abs(try_df$dbh - (try_df$a + try_df$dbh_total_growth*p)) == min(abs(try_df$dbh - (try_df$a + try_df$dbh_total_growth*p))))]
#     try_df$perc[try] <- p # assign the number to the value found above
#     percs_new <- rbind(percs_new, try_df)
#   }
# }
#
# percs_new_sub <- percs_new[percs_new$perc %in% c(0.05, 0.25,0.50,0.75),]
# percs_new_sub$tag_year_perc <- paste0(percs_new_sub$tag_year, percs_new_sub$perc)
# percs_new_sub <- percs_new_sub[!(duplicated(percs_new_sub$tag_year_perc)),]
# percs_new_sub <- percs_new_sub[,c(23,16)]
#
# Wood_pheno_table_hf$tag_year_perc <- paste0(Wood_pheno_table_hf$tag_year, Wood_pheno_table_hf$perc)
# Wood_pheno_table_hf <- left_join(Wood_pheno_table_hf, percs_new_sub, by = "tag_year_perc")
#
# write.csv(Wood_pheno_table_hf, file = "data/dendrobands/HF/modeled/Wood_table_hf_updated.csv")

#Add start of growth (5%)
# percs_new <- data.frame(NULL)
# for(i in 1:length(unique(percent_growth$tag_year))){
#   try_df <-percent_growth[percent_growth$tag_year %in% unique(percent_growth$tag_year)[i],]
#   try_df$perc <- seq(1,365,1)
#
#   for (p in c(.05)) {
#     try <- try_df$doy[which(abs(try_df$dbh - (try_df$a + try_df$dbh_total_growth*p)) == min(abs(try_df$dbh - (try_df$a + try_df$dbh_total_growth*p))))]
#     try_df$perc[try] <- p # assign the number to the value found above
#     percs_new <- rbind(percs_new, try_df)
#     percs_new <- percs_new[percs_new$perc %in% 0.05,]
#
#       }
# }
#
# percs_new_sub <- percs_new[percs_new$perc %in% 0.05,]
# percs_new_sub$tag_year_perc <- paste0(percs_new_sub$tag_year, percs_new_sub$perc)
# percs_new_sub <- percs_new_sub[!(duplicated(percs_new_sub$tag_year_perc)),]
# percs_new_sub <- percs_new_sub[,c(3,23,16)]
#
# Wood_pheno_table_hf <- read_csv("Data/dendrobands/HF/modeled/Wood_table_hf_updated.csv")
# Wood_pheno_table_hf <- Wood_pheno_table_hf[,-1]
#
# new_wood_table <- data.frame(NULL)
# for(i in 1:nrow(percs_new_sub)){
# wood_sub <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% percs_new_sub$tag_year[i],]
# new_row <- wood_sub[1,]
# new_row$perc <- 0.05
# new_row$doy <- percs_new_sub[i,]$doy
# wood_sub <- rbind(wood_sub, new_row)
# new_wood_table <- rbind(new_wood_table,wood_sub)
# }
#
# write.csv(new_wood_table, file = "data/dendrobands/HF/modeled/Wood_table_hf_updated.csv")
#
##############
#5994 -> 4254 =
Wood_pheno_table_hf <- read_csv("Data/dendrobands/HF/modeled/Wood_table_hf_updated.csv")
Wood_pheno_table_hf <- Wood_pheno_table_hf[,-1]
Wood_pheno_table_hf <- Wood_pheno_table_hf[!(Wood_pheno_table_hf$perc %in% 0.05),]
Wood_pheno_table_hf <- Wood_pheno_table_hf[!(Wood_pheno_table_hf$year %in% 1998),]
#Remove tag years where doy% variable is >SD away from mean
Wood_pheno_table_hf <- Wood_pheno_table_hf[!(is.na(Wood_pheno_table_hf$doy)),]
means <- aggregate(Wood_pheno_table_hf$doy, by = list(Wood_pheno_table_hf$perc,Wood_pheno_table_hf$year, Wood_pheno_table_hf$wood_type), FUN = mean)
SD <- aggregate(Wood_pheno_table_hf$doy, by = list(Wood_pheno_table_hf$perc,Wood_pheno_table_hf$year, Wood_pheno_table_hf$wood_type), FUN = sd)
names(SD) <- c("Group.1", "Group.2","Group.3", "sd")
means <- left_join(means,SD)
names(means) <- c("perc","year", "wood_type", "mean", "SD")

Wood_pheno_table_hf <- left_join(Wood_pheno_table_hf, means, by = c("perc","year", "wood_type"))

Wood_pheno_table_hf$rm <- ifelse(Wood_pheno_table_hf$doy > Wood_pheno_table_hf$mean+sd*Wood_pheno_table_hf$SD | Wood_pheno_table_hf$doy < Wood_pheno_table_hf$mean-sd*Wood_pheno_table_hf$SD, "rm", "keep")
bad_doys <- as.character(unique(Wood_pheno_table_hf[Wood_pheno_table_hf$rm %in% "rm",]$tag_year)) #27
bad_doy_tags <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% bad_doys,]

Wood_pheno_table_hf <- Wood_pheno_table_hf[Wood_pheno_table_hf$rm %in% "keep",]
perc_check <- aggregate(Wood_pheno_table_hf$perc, by = list(Wood_pheno_table_hf$tag_year), FUN = sum)
perc_check <- perc_check[perc_check$x == 1.50,]
Wood_pheno_table_hf <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% perc_check$Group.1,]

percent_growth <- percent_growth[percent_growth$tag_year %in% Wood_pheno_table_hf$tag_year,]

#
#Remove years where raw measures past doy 250 were >20% of total growth
# #all_stems <- read_csv("Data/dendrobands/SCBI/raw_data/all_stems.csv")
# all_stems <- read_csv("Data/dendrobands/HF/raw_data/HarvardDendroband_cleaned.csv")
# library(lubridate)
# all_stems$date <- as.Date(all_stems$date, format = "%m/%d/%Y")
# all_stems$year <- year(all_stems$date)
#
# all_stems$tag_year <- paste0(all_stems$plot,all_stems$tag, all_stems$year)
# min <- aggregate(all_stems$`Diameter (cm)`, by = list(all_stems$tag_year), FUN = min)
# names(min) <- c("tag_year","min")
# max <- aggregate(all_stems$`Diameter (cm)`, by = list(all_stems$tag_year), FUN = max)
# names(max) <- c("tag_year","max")
# raw_total <- left_join(min, max)
# raw_total$tot <- raw_total$max-raw_total$min
# raw_total <- raw_total[,c(-2,-3)]
# all_stems <- left_join(all_stems, raw_total, by = "tag_year")
#
# new_stems <- data.frame(NULL)
#
# tag_years <- unique(all_stems$tag_year)
#
# for(i in 1:length(tag_years)){
#
#   sub_stems <- all_stems[all_stems$tag_year %in% tag_years[i],]
#   sub_stems$growth <-  sub_stems$`Diameter (cm)` - lag(sub_stems$`Diameter (cm)`)
#   new_stems <- rbind(new_stems, sub_stems)
#
# }
# new_stems$doy <- yday(new_stems$date)
# late_growth <- new_stems[new_stems$doy >= 250,]
# late_growth$perc_growth <- late_growth$growth/late_growth$tot
# late_growth_rm <- late_growth[late_growth$perc_growth > 0.30,]
#
# percent_growth <- percent_growth[!(percent_growth$tag_year %in% late_growth_rm$tag_year),]
# Wood_pheno_table_hf <-Wood_pheno_table_hf[!(Wood_pheno_table_hf$tag_year %in% late_growth_rm$tag_year),]
#
# #Remove years where first measurement occured after doy 126 ~May (start date in 2001)
all_stems <- read_csv("Data/dendrobands/HF/raw_data/HarvardDendroband_cleaned.csv")
library(lubridate)
all_stems$date <- as.Date(all_stems$date, format = "%m/%d/%Y")
all_stems$year <- year(all_stems$date)
all_stems$doy <- yday(all_stems$date)
all_stems$tag_year <- paste0(all_stems$plot,all_stems$tag, all_stems$year)

#firsts <- aggregate(all_stems$doy, by = list(all_stems$tag_year), FUN = min)
#ID where latest spring survey occured
latest_day <- 126

# Wood_pheno_table_hf <- Wood_pheno_table_hf[!(Wood_pheno_table_hf$year %in% 1998),]
# aggregate(Wood_pheno_table_hf$doy, by = list(Wood_pheno_table_hf$perc), FUN = mean)#25 = 159.4, 50 = 178.5, 75 = 199

#remove stem years where first survey was missed
library(lubridate)

start_days <- aggregate(all_stems$doy, by = list(all_stems$tag_year), FUN = min)
start_days <- start_days[start_days$x > latest_day,]

#Look at the tree years
late_starts <- all_stems[all_stems$tag_year %in% start_days$Group.1,]

#remove from percent growth and wood_pheno
percent_growth <- percent_growth[!(percent_growth$tag_year %in% late_starts$tag_year),]
late_survey <- Wood_pheno_table_hf[(Wood_pheno_table_hf$tag_year %in% late_starts$tag_year),]

Wood_pheno_table_hf <- Wood_pheno_table_hf[!(Wood_pheno_table_hf$tag_year %in% late_starts$tag_year),]

#Subset percent growth to only contain modeled growth in b-a
percent_growth <- subset(percent_growth, percent_growth$dbh > percent_growth$a & percent_growth$dbh < percent_growth$b)

######################################################
#percent_growth <- percent_growth[!(percent_growth$year %in% 1999),]
## Clean the data ----
# Break into RP and DP
percent_growth_RP <- subset(percent_growth, wood_type == "ring-porous")

# first_days <- aggregate(percent_growth_RP$doy, by = list(percent_growth_RP$tag_year), FUN= min)
# names(first_days) <- c("tag_year", "start")
# mean(first_days$start) + (2*sd(first_days$start))
# plot(first_days$start)
#
# #Wood_pheno_table_hf_rp <- Wood_pheno_table_hf[Wood_pheno_table_hf$wood_type %in% "ring-porous",]
# # start <- Wood_pheno_table_hf_rp[Wood_pheno_table_hf_rp$perc %in% 0.05,]
# # start <- start[,c(14,16)]
# # names(start) <- c("tag_year", "05")
#
# twentyfive <- Wood_pheno_table_hf[Wood_pheno_table_hf$perc %in% 0.25,]
# twentyfive <- twentyfive[,c(14,16)]
# names(twentyfive) <- c("tag_year", "25")
#
# fifty <- Wood_pheno_table_hf[Wood_pheno_table_hf$perc %in% 0.50,]
# fifty <- fifty[,c(14,16)]
# names(fifty) <- c("tag_year", "50")
#
# seventyfive <- Wood_pheno_table_hf[Wood_pheno_table_hf$perc %in% 0.75,]
# seventyfive <- seventyfive[,c(14,16)]
# names(seventyfive) <- c("tag_year", "75")
#
# first_days <- left_join(start, twentyfive, by = "tag_year")
# first_days <- left_join(first_days, fifty, by = "tag_year")
# first_days <- left_join(first_days, seventyfive, by = "tag_year")
#
# library(reshape2)
# first_days <- melt(first_days, id.vars = "tag_year")
# HF_RP <- ggplot(first_days, aes( x = tag_year, y = value, color = variable)) +
#   geom_point()
# Remove models with single days growth >2 SD away from mean (this is abnormal for RP trees in our study)
maxgrowthrate <- aggregate(percent_growth_RP$dbh_growth_percent, by = list(percent_growth_RP$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate") # Maximum growth rate of each tree
# high_growthday <- subset(maxgrowthrate, rate >= 0.02) #Remove trees where one day had higher than 2% of total growth
high_growthday <- subset(maxgrowthrate, rate >= (mean(maxgrowthrate$rate + (sd(maxgrowthrate$rate) * sd))) | rate <= 0) # Remove trees where one day had higher than 4% of total growth

# ENTRIES REMOVED IN THIS STEP
percent_growth_high <- percent_growth_RP[percent_growth_RP$tag_year %in% high_growthday$tag_year, ]
HF_highgrowth_RP <- unique(percent_growth_high$tag_year)
wood_pheno_removed <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% HF_highgrowth_RP, ]
wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
count(wood_pheno_removed, sp)
hist(wood_pheno_removed$dbh)
maxgrowthrate_removed <- maxgrowthrate[maxgrowthrate$tag_year %in% HF_highgrowth_RP, ]
hist(maxgrowthrate_removed$rate)

# Subset main DF
percent_growth_RP <- percent_growth_RP[!(percent_growth_RP$tag_year %in% unique(high_growthday$tag_year)), ]

# Remove models with peak growth outside of expected season. IE, winter growth peaks
ratedoy <- percent_growth_RP[percent_growth_RP$dbh_growth_percent %in% maxgrowthrate$rate, ] # doy where max growth occured
keeptags <- NULL
for (i in 1999:2003) {
  year <- subset(ratedoy, year == i)
  upper <- (mean(year$doy) + (sd * sd(year$doy)))
  lower <- (mean(year$doy) - (sd * sd(year$doy)))
  year <- subset(year, doy <= upper & doy >= lower) # Remove models where peak growth was in winter
  # year <- year[year$tag_year %in% ratedoysubset$tag_year,]
  goodtags <- unique(year$tag_year)
  keeptags <- append(keeptags, goodtags)
}
percent_growth_RP_gone <- percent_growth_RP[!(percent_growth_RP$tag_year %in% keeptags),]
HF_badpeak_RP <- unique(percent_growth_RP_gone$tag_year)

percent_growth_RP <- percent_growth_RP[percent_growth_RP$tag_year %in% keeptags,]
#ENTRIES REMOVED IN THIS STEP
#percent_growth_badpeak <- percent_growth_RP[!(percent_growth_RP$tag_year %in% ratedoysubset$tag_year),]
#HF_badpeak_RP <- unique(percent_growth_badpeak$tag_year)
#wood_pheno_removed <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% HF_badpeak_RP,]
#wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
#count(wood_pheno_removed,sp)
#hist(wood_pheno_removed$dbh)

#subset main DF
#percent_growth_RP <- percent_growth_RP[percent_growth_RP$tag_year %in% ratedoysubset$tag_year,]

#Diffuse-porous - Same deal
percent_growth_DP <- subset(percent_growth, wood_type == "diffuse-porous")

# first_days <- aggregate(percent_growth_DP$doy, by = list(percent_growth_DP$tag_year), FUN= min)
# names(first_days) <- c("tag_year", "start")
# mean(first_days$start) + (2*sd(first_days$start))
# plot(first_days$start)

# Wood_pheno_table_hf_dp <- Wood_pheno_table_hf[Wood_pheno_table_hf$wood_type %in% "diffuse-porous",]
# start <- Wood_pheno_table_hf_dp[Wood_pheno_table_hf_dp$perc %in% 0.05,]
# start <- start[,c(14,16)]
# names(start) <- c("tag_year", "05")
#
# twentyfive <- Wood_pheno_table_hf[Wood_pheno_table_hf$perc %in% 0.25,]
# twentyfive <- twentyfive[,c(14,16)]
# names(twentyfive) <- c("tag_year", "25")
#
# fifty <- Wood_pheno_table_hf[Wood_pheno_table_hf$perc %in% 0.50,]
# fifty <- fifty[,c(14,16)]
# names(fifty) <- c("tag_year", "50")
#
# seventyfive <- Wood_pheno_table_hf[Wood_pheno_table_hf$perc %in% 0.75,]
# seventyfive <- seventyfive[,c(14,16)]
# names(seventyfive) <- c("tag_year", "75")
#
# first_days <- left_join(start, twentyfive, by = "tag_year")
# first_days <- left_join(first_days, fifty, by = "tag_year")
# first_days <- left_join(first_days, seventyfive, by = "tag_year")
#
# library(reshape2)
# first_days <- melt(first_days, id.vars = "tag_year")
# HF_DP <- ggplot(first_days, aes( x = tag_year, y = value, color = variable)) +
#   geom_point()
# Remove models with single days > 2% growth (this is abnormal for RP trees in our study)
maxgrowthrate <- aggregate(percent_growth_DP$dbh_growth_percent, by = list(percent_growth_DP$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate") # Maximum growth rate of each tree
# high_growthday <- subset(maxgrowthrate, rate >= 0.04) #Remove trees where one day had higher than 4% of total growth
high_growthday <- subset(maxgrowthrate, rate >= (mean(maxgrowthrate$rate + (sd(maxgrowthrate$rate) * sd))) | rate <= 0) # Remove trees where one day had higher than 4% of total growth

# ENTRIES REMOVED IN THIS STEP
percent_growth_high <- percent_growth_DP[percent_growth_DP$tag_year %in% high_growthday$tag_year, ]
HF_highgrowth_DP <- unique(percent_growth_high$tag_year)
wood_pheno_removed <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% HF_highgrowth_DP, ]
wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
count(wood_pheno_removed, sp)
hist(wood_pheno_removed$dbh)

# Subset main DF
percent_growth_DP <- percent_growth_DP[!(percent_growth_DP$tag_year %in% unique(high_growthday$tag_year)), ]

# Remove models with peak growth outside of expected season. IE, winter growth peaks
ratedoy <- percent_growth_DP[percent_growth_DP$dbh_growth_percent %in% maxgrowthrate$rate, ] # doy where max growth occured
keeptags <- NULL
for (i in 1999:2003) {
  year <- subset(ratedoy, year == i)
  upper <- (mean(year$doy) + (sd * sd(year$doy)))
  lower <- (mean(year$doy) - (sd * sd(year$doy)))
  year <- subset(year, doy <= upper & doy >= lower) # Remove models where peak growth was in winter
  # year <- year[year$tag_year %in% ratedoysubset$tag_year,]
  goodtags <- unique(year$tag_year)
  keeptags <- append(keeptags, goodtags)
}
percent_growth_DP_gone <- percent_growth_DP[!(percent_growth_DP$tag_year %in% keeptags),]
HF_badpeak_DP <- unique(percent_growth_DP_gone$tag_year)

percent_growth_DP <- percent_growth_DP[percent_growth_DP$tag_year %in% keeptags,]
#ENTRIES REMOVED IN THIS STEP
#percent_growth_badpeak <- percent_growth_DP[!(percent_growth_DP$tag_year %in% ratedoysubset$tag_year),]
#HF_badpeak_DP <- unique(percent_growth_badpeak$tag_year)
#wood_pheno_removed <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% HF_badpeak_DP,]
#wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
#count(wood_pheno_removed,sp)
#hist(wood_pheno_removed$dbh)

#subset main DF
#percent_growth_DP <- percent_growth_DP[percent_growth_DP$tag_year %in% ratedoysubset$tag_year,]

#Re-combine into single DF
percent_growth <- rbind(percent_growth_DP, percent_growth_RP)

## Clean both at once now ----
# Remove models with small or negligible growth
# ENTRIES REMOVED THIS STEP
percent_growth_gone <- subset(percent_growth, dbh_total_growth <= 0.05) # remove small growth trees
HF_smallgrowth <- unique(percent_growth_gone$tag_year)
wood_pheno_removed <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% HF_smallgrowth, ]
wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
count(wood_pheno_removed, sp)
hist(wood_pheno_removed$dbh)
# Subset the main DF
percent_growth <- subset(percent_growth, dbh_total_growth >= 0.05) # remove small growth trees


# # Remove models that failed to model at least 99% of total growth using LG5.pre
# percent_growth_one <- subset(percent_growth, dbh_growth_percent_cummulative >= .975) # Find tags that reach ~100% growth
# unique(percent_growth_one$tag_year) # Check how many we have
# # ENTRIES REMOVED IN THIS STEP
# percent_growth_low <- percent_growth[!(percent_growth$tag_year %in% unique(percent_growth_one$tag_year)), ]
# HF_below99 <- unique(percent_growth_low$tag_year)
# wood_pheno_removed <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% HF_below99, ]
# wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
# count(wood_pheno_removed, sp)
# hist(wood_pheno_removed$dbh)
# LG5_parameter_values_removed_99_hf <- LG5_parameter_values_hf[LG5_parameter_values_hf$tag_year %in% HF_below99, ]
#
# # Subset main DF
# percent_growth <- percent_growth[(percent_growth$tag_year %in% unique(percent_growth_one$tag_year)), ] # Remove tags with <99% growth

### Messing around ###
# percent_growth$check <- 1
# percent_growth_season_length <- subset(percent_growth, dbh_growth_percent >= 0.01)
# SL <- aggregate(percent_growth_season_length$check, by = list(percent_growth_season_length$tag_year), FUN = sum)
# SL20 <- subset(SL, x <=5)
# percent_growth_SL <- percent_growth[percent_growth$tag_year %in% "D15272002",]
# percent_growth_5 <- subset(percent_growth, dbh_growth_percent >= 0.05)

percent_growth_na <- percent_growth[(is.na(percent_growth$b)),]
HF_na <- unique(percent_growth_na$tag_year)
percent_growth <- percent_growth[!(is.na(percent_growth$b)),]

# percent_growth_DP_try <- percent_growth_DP %>%
#  group_by(tag_year) %>%
#  mutate(spikecheck = dbh_growth/lag(dbh_growth))
#percent_growth_two_tags <-subset(percent_growth_DP_try, spikecheck >= 7 & spikecheck<= 100)
#percent_growth_two <- percent_growth[percent_growth$tag_year %in% unique(percent_growth_two_tags$tag_year),]
#Check for patterns in removed trees
removed_tagyears <- c(HF_badpeak_DP,HF_badpeak_RP,HF_highgrowth_DP,HF_highgrowth_RP,HF_smallgrowth, HF_na)
removed_trees <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% removed_tagyears,]
removed_trees <- rbind(removed_trees, bad_doy_tags, late_survey)
removed_trees <-subset(removed_trees, perc == .25)
hist(removed_trees$dbh)
HF_removed_tags_DF <- count(removed_trees,sp)

Wood_pheno_table_hf_25 <- subset(Wood_pheno_table_hf, perc == .25)
HF_total_tags_DF <- count(Wood_pheno_table_hf_25, sp)

library(gridExtra)
png(filename = "doc/manuscript/tables_figures/data_cleaning_figure_hf.png", width=15, height=10,
    pointsize=12, bg="transparent", units="in", res=600,
    restoreConsole=FALSE)

grid.arrange(

ggplot(Wood_pheno_table_hf_25, aes(x = dbh, fill = sp))+geom_histogram(position = "dodge", binwidth = 20)+
  labs(x = "DBH", y = "# of tag years", title = "Tag years pre-removal", fill = "")+
  theme_bw()+
  theme(legend.position = "none"),

ggplot(removed_trees, aes(x = dbh, fill = sp))+geom_histogram(position = "dodge", binwidth = 20)+
  labs(x = "DBH", y = "", title = "Tag years removed", fill = "Species code")+
  theme_bw()+
  theme(legend.position = c(.9,.60)),

ggplot(Wood_pheno_table_hf_25, aes(x = sp, fill = sp))+geom_bar(position = "dodge")+
  labs(x = "Species", y = "# of tag years", title = "", fill = "")+
  theme_bw()+
  theme(legend.position = "none"),

ggplot(removed_trees, aes(x = sp, fill = sp))+geom_bar(position = "dodge")+
  labs(x = "Species", y = "", title = "", fill = "")+
  theme_bw()+
  theme(legend.position = "none"),

as.table = TRUE, nrow=2, ncol=2) ###as.table specifies order if multiple rows

dev.off()



#Remove bad models from wood_pheno_table and LG5_param DF's ----
Wood_pheno_table_hf$tag_year <- paste0(Wood_pheno_table_hf$tag, Wood_pheno_table_hf$year)
Wood_gone <- Wood_pheno_table_hf[!(Wood_pheno_table_hf$tag_year %in% unique(percent_growth$tag_year)), ]

Wood_pheno_table_hf <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% unique(percent_growth$tag_year), ]
tot_growth <- distinct(percent_growth[, c(3, 19)], .keep_all = TRUE)
Wood_pheno_table_hf <- left_join(Wood_pheno_table_hf, tot_growth, by = "tag_year")

LG5_parameter_values_hf$tag_year <- paste0(LG5_parameter_values_hf$site_tag, LG5_parameter_values_hf$year)
LG5_parameter_values_hf <- LG5_parameter_values_hf[LG5_parameter_values_hf$tag_year %in% unique(percent_growth$tag_year), ]
# V6 - 99% V7 95% V8 put % check last V9 97.5%
write.csv(Wood_pheno_table_hf, file = "data/dendrobands/HF/modeled/Wood_pheno_table_HarvardForest_CLEAN.csv", row.names = FALSE)
write.csv(LG5_parameter_values_hf, file = "data/dendrobands/HF/modeled/LG5_parameter_values_HarvardForest_CLEAN.csv", row.names = FALSE)

## Harvard Forest plots ----
rel_growth_hf <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent, group = tag_year)) +
  coord_cartesian(ylim = c(0, 0.05)) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "DOY", y = "Percent of total growth",
    title = "Percent of total (modeled) growth in diameter",
    subtitle = "Each curve represents one tag-year"
  ) +
  theme_bw() +
  geom_line(alpha = 0.2, aes(col = wood_type)) +
  scale_colour_viridis_d("Wood Type", end = 2 / 3)
rel_growth_hf
# fig_width <- 7
# ggsave(filename = "doc/manuscript/tables_figures/rel_growth.png",
#       plot = rel_growth,
#       width = fig_width, height = fig_width / 2)



fig3_hf <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent_cummulative, group = tag_year, col = wood_type)) +
  geom_line(alpha = 0.2) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Cummulative percent of total growth", title = "Cummulative percent of total (modeled) growth in diameter")
fig3_hf

library(patchwork)
(SCBI_RP + SCBI_DP) / (HF_RP + HF_DP)
