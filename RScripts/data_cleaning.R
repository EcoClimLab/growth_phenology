# Setup ------------------------------------------------------------------------
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
Wood_pheno_table_scbi <- read_csv("Data/Wood_pheno_table_V10RAW.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))
Wood_pheno_table_scbi$tag_year_perc <- paste0(Wood_pheno_table_scbi$tag, Wood_pheno_table_scbi$year, Wood_pheno_table_scbi$perc)
unitag <- unique(Wood_pheno_table_scbi)


LG5_parameter_values_scbi <- read_csv("Data/LG5_parameter_values_V10RAW.csv")

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
  mutate(doy = list(seq(from = 1, to = 365, by = 1))) %>%
  unnest_longer(doy) %>%
  group_by(tag_year) %>%
  mutate(
    dbh = lg5(L, K, doy_ip, r, theta, doy),
    dbh_growth = dbh - lag(dbh),
    dbh_total_growth = K - L,
    dbh_growth_percent = dbh_growth / dbh_total_growth
  ) %>%
  filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )

Wood_pheno_table_scbi <- Wood_pheno_table_scbi %>%
  separate(tag, into = c("tag", "stem"), sep = "_") %>%
  mutate(
    tag = as.numeric(tag),
    tag_year = str_c(tag, year)
  )

## Clean the data ----
# Break into RP and DP
percent_growth_RP <- subset(percent_growth, wood_type == "ring-porous")

# Remove models with single days > 2% growth (this is abnormal for RP trees in our study)
maxgrowthrate <- aggregate(percent_growth_RP$dbh_growth_percent, by = list(percent_growth_RP$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate") # Maximum growth rate of each tree
# high_growthday <- subset(maxgrowthrate, rate >= 0.02) #Remove trees where one day had higher than 2% of total growth
high_growthday <- subset(maxgrowthrate, rate >= (mean(maxgrowthrate$rate + (sd(maxgrowthrate$rate) * sd))) | rate <= 0) # Remove trees where one day had higher than 4% of total growth

# ENTRIES REMOVED IN THIS STEP
percent_growth_high <- percent_growth_RP[percent_growth_RP$tag_year %in% high_growthday$tag_year, ]
SCBI_highgrowth_RP <- unique(percent_growth_high$tag_year)
wood_pheno_removed <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% SCBI_highgrowth_RP, ]
wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
count(wood_pheno_removed, sp)
hist(wood_pheno_removed$dbh)
# subset main DF
percent_growth_RP <- percent_growth_RP[!(percent_growth_RP$tag_year %in% unique(high_growthday$tag_year)), ]


# Remove models with peak growth outside of expected season. IE, winter growth peaks
ratedoy <- percent_growth_RP[percent_growth_RP$dbh_growth_percent %in% maxgrowthrate$rate, ] # DOY where max growth occured
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
percent_growth_RP <- percent_growth_RP[percent_growth_RP$tag_year %in% keeptags, ]
# ENTRIES REMOVED IN THIS STEP
# percent_growth_badpeak <- percent_growth_RP[!(percent_growth_RP$tag_year %in% ratedoysubset$tag_year),]
# SCBI_badpeak_RP <- unique(percent_growth_badpeak$tag_year)
# wood_pheno_removed <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% SCBI_badpeak_RP,]
# wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
# count(wood_pheno_removed,sp)
# hist(wood_pheno_removed$dbh)

# subset main DF
# percent_growth_RP <- percent_growth_RP[percent_growth_RP$tag_year %in% ratedoysubset$tag_year,]

# DIffuse porous - Same method
percent_growth_DP <- subset(percent_growth, wood_type == "diffuse-porous")

# Remove models with single days > 4% growth (this is abnormal for DP trees in our study)
maxgrowthrate <- aggregate(percent_growth_DP$dbh_growth_percent, by = list(percent_growth_DP$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate") # Maximum growth rate of each tree
high_growthday <- subset(maxgrowthrate, rate >= (mean(maxgrowthrate$rate) + (sd(maxgrowthrate$rate) * sd)) & rate <= 0) # Remove trees where one day had higher than 4% of total growth
# ENTRIES REMOVED IN THIS STEP
# percent_growth_high <- percent_growth_DP[percent_growth_DP$tag_year %in% high_growthday$tag_year,]
# SCBI_highgrowth_DP <- unique(percent_growth_high$tag_year)
# wood_pheno_removed <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% SCBI_highgrowth_DP,]
# wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
# count(wood_pheno_removed,sp)
# hist(wood_pheno_removed$dbh)

# Subset main DF
percent_growth_DP <- percent_growth_DP[!(percent_growth_DP$tag_year %in% unique(high_growthday$tag_year)), ]

# Remove models with peak growth outside of expected season. IE, winter growth peaks
ratedoy <- percent_growth_DP[percent_growth_DP$dbh_growth_percent %in% maxgrowthrate$rate, ] # DOY where max growth occured
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
percent_growth_DP <- percent_growth_DP[percent_growth_DP$tag_year %in% keeptags, ]
# ENTRIES REMOVED IN THIS STEP
# percent_growth_badpeak <- percent_growth_DP[!(percent_growth_DP$tag_year %in% ratedoysubset$tag_year),]
# SCBI_badpeak_DP <- unique(percent_growth_badpeak$tag_year)
# wood_pheno_removed <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% SCBI_badpeak_DP,]
# wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
# count(wood_pheno_removed,sp)
# hist(wood_pheno_removed$dbh)

# subset main DF
# percent_growth_DP <- percent_growth_DP[percent_growth_DP$tag_year %in% ratedoysubset$tag_year,]

# Re-combine into single DF
percent_growth <- rbind(percent_growth_DP, percent_growth_RP)

## Clean both at once now ----
# Remove small/negligable growth models...Seem to be more of a problem with Harvard Forest but remove here for consistency
percent_growth_gone <- subset(percent_growth, dbh_total_growth <= 0.02) # remove small growth trees
SCBI_smallgrowth <- unique(percent_growth_gone$tag_year)
percent_growth <- subset(percent_growth, dbh_total_growth >= 0.02) # remove small growth trees
# Remove models that failed to model at least 99% of total growth using LG5.pred
percent_growth_one <- subset(percent_growth, dbh_growth_percent_cummulative >= .975) # Find tags that reach ~100% growth
unique(percent_growth_one$tag_year) # Check how many tags meet this req
# ENTRIES REMOVED IN THIS STEP
percent_growth_low <- percent_growth[!(percent_growth$tag_year %in% unique(percent_growth_one$tag_year)), ]
SCBI_below99 <- unique(percent_growth_low$tag_year)
wood_pheno_removed <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% SCBI_below99, ]
wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
count(wood_pheno_removed, sp)
ggplot(wood_pheno_removed, aes(x = dbh, group = sp, fill = sp)) +
  geom_histogram(position = "dodge")
ggplot(Wood_pheno_table_scbi, aes(x = dbh, group = sp, fill = sp)) +
  geom_histogram(position = "dodge")

hist(wood_pheno_removed$year)
LG5_parameter_values_removed_99_scbi <- LG5_parameter_values_scbi[LG5_parameter_values_scbi$tag_year %in% SCBI_below99, ]
hist(LG5_parameter_values_scbi$ML_value)
hist(LG5_parameter_values_removed$ML_value)

# Subset main DF
percent_growth <- percent_growth[(percent_growth$tag_year %in% unique(percent_growth_one$tag_year)), ] # Remove tags with <99% growth

# Strange spikes
percent_growth_na <- percent_growth[(is.na(percent_growth$b)), ]

percent_growth <- percent_growth[!(is.na(percent_growth$b)), ]

## Check for patterns in removed trees ----
removed_tagyears <- c(SCBI_badpeak_DP, SCBI_badpeak_RP, SCBI_below99, SCBI_highgrowth_DP, SCBI_highgrowth_RP, SCBI_smallgrowth)
removed_trees <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% removed_tagyears, ]
removed_trees <- subset(removed_trees, perc == .25)
hist(removed_trees$dbh)
count(removed_trees, sp)

weirdtag <- subset(percent_growth, doy == 190)
weirdtag <- subset(weirdtag, dbh_growth_percent == max(weirdtag$dbh_growth_percent))

percent_growth <- percent_growth[!(percent_growth$tag_year %in% unique(weirdtag$tag_year)), ] # Remove tags with <99% growth

weirdtag <- subset(percent_growth, doy == 353:365)
weirdtag <- subset(weirdtag, dbh_growth_percent == max(weirdtag$dbh_growth_percent))

percent_growth <- percent_growth[!(percent_growth$tag_year %in% unique(weirdtag$tag_year)), ] # Remove tags with <99% growth

## Remove bad models from wood_pheno_table and LG5_param DF's ----
# Wood_pheno_table_scbi <-  Wood_pheno_table_scbi %>%
#  separate(tag, into = c("tag", "stem"), sep = "_") %>%
#  mutate(
#    tag = as.numeric(tag),
#    tag_year = str_c(tag, year)
#  )
# unitag <- unique(Wood_pheno_table_scbi)
wood_gone <- Wood_pheno_table_scbi[!(Wood_pheno_table_scbi$tag_year %in% unique(percent_growth$tag_year)), ]
wood_gone <- subset(wood_gone, perc == .25)
247 / (3348 / 3) # 1116

Wood_pheno_table_scbi <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% unique(percent_growth$tag_year), ]
tot_growth <- distinct(percent_growth[, c(3, 16)], .keep_all = TRUE)
Wood_pheno_table_scbi <- left_join(Wood_pheno_table_scbi, tot_growth, by = "tag_year")
# unitag <- unique(Wood_pheno_table_scbi)

LG5_parameter_values_scbi$tag_year <- paste0(LG5_parameter_values_scbi$tag, LG5_parameter_values_scbi$year)
LG5_parameter_values_scbi <- LG5_parameter_values_scbi[LG5_parameter_values_scbi$tag_year %in% unique(percent_growth$tag_year), ]

# V11 = 99%, V12 = 95%, V13 = 97.5%
write.csv(Wood_pheno_table_scbi, file = "data/Wood_pheno_table_V13CLEAN.csv", row.names = FALSE)
write.csv(LG5_parameter_values_scbi, file = "data/LG5_parameter_values_V13CLEAN.csv", row.names = FALSE)

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



fig3_scbi <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent_cummulative, group = tag_year, col = wood_type)) +
  geom_line(alpha = 0.2) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Cummulative percent of total growth", title = "Cummulative percent of total (modeled) growth in diameter")
fig3_scbi

# fig_width <- 6
# ggsave(filename = "doc/manuscript/tables_figures/fig3.png", plot = fig3, width = fig_width, height = fig_width / 1.52)

# Harvard Forest ---------------------------------------------------------------
## Load up percent growth DF ----
sd <- 2
Wood_pheno_table_hf <- read_csv("Data/Wood_pheno_table_HarvardForest_V4.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

Wood_pheno_table_hf$site_tag <- Wood_pheno_table_hf$tag
Wood_pheno_table_hf$site <- substr(Wood_pheno_table_hf$tag, 1, 2)
Wood_pheno_table_hf$realtag <- substr(Wood_pheno_table_hf$site_tag, 3, nchar(as.character(Wood_pheno_table_hf$site_tag)))

LG5_parameter_values_hf <- read_csv("Data/LG5_parameter_values_HarvardForest_V3.csv")
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
  mutate(doy = list(seq(from = 1, to = 365, by = 1))) %>%
  unnest_longer(doy) %>%
  group_by(tag_year) %>%
  mutate(
    dbh = lg5(L, K, doy_ip, r, theta, doy),
    dbh_growth = dbh - lag(dbh),
    dbh_total_growth = K - L,
    dbh_growth_percent = dbh_growth / dbh_total_growth
  ) %>%
  filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )
Wood_pheno_table_hf$tag_year <- paste0(Wood_pheno_table_hf$tag, Wood_pheno_table_hf$year)

## Clean the data ----
# Break into RP and DP
percent_growth_RP <- subset(percent_growth, wood_type == "ring-porous")

# Remove models with single days > 2% growth (this is abnormal for RP trees in our study)
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
ratedoy <- percent_growth_RP[percent_growth_RP$dbh_growth_percent %in% maxgrowthrate$rate, ] # DOY where max growth occured
keeptags <- NULL
for (i in 1998:2003) {
  year <- subset(ratedoy, year == i)
  upper <- (mean(year$doy) + (sd * sd(year$doy)))
  lower <- (mean(year$doy) - (sd * sd(year$doy)))
  year <- subset(year, doy <= upper & doy >= lower) # Remove models where peak growth was in winter
  # year <- year[year$tag_year %in% ratedoysubset$tag_year,]
  goodtags <- unique(year$tag_year)
  keeptags <- append(keeptags, goodtags)
}
percent_growth_RP <- percent_growth_RP[percent_growth_RP$tag_year %in% keeptags, ]
# ENTRIES REMOVED IN THIS STEP
# percent_growth_badpeak <- percent_growth_RP[!(percent_growth_RP$tag_year %in% ratedoysubset$tag_year),]
# HF_badpeak_RP <- unique(percent_growth_badpeak$tag_year)
# wood_pheno_removed <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% HF_badpeak_RP,]
# wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
# count(wood_pheno_removed,sp)
# hist(wood_pheno_removed$dbh)

# subset main DF
# percent_growth_RP <- percent_growth_RP[percent_growth_RP$tag_year %in% ratedoysubset$tag_year,]

# Diffuse-porous - Same deal
percent_growth_DP <- subset(percent_growth, wood_type == "diffuse-porous")

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
ratedoy <- percent_growth_DP[percent_growth_DP$dbh_growth_percent %in% maxgrowthrate$rate, ] # DOY where max growth occured
keeptags <- NULL
for (i in 1998:2003) {
  year <- subset(ratedoy, year == i)
  upper <- (mean(year$doy) + (sd * sd(year$doy)))
  lower <- (mean(year$doy) - (sd * sd(year$doy)))
  year <- subset(year, doy <= upper & doy >= lower) # Remove models where peak growth was in winter
  # year <- year[year$tag_year %in% ratedoysubset$tag_year,]
  goodtags <- unique(year$tag_year)
  keeptags <- append(keeptags, goodtags)
}
percent_growth_DP <- percent_growth_DP[percent_growth_DP$tag_year %in% keeptags, ]
# ENTRIES REMOVED IN THIS STEP
# percent_growth_badpeak <- percent_growth_DP[!(percent_growth_DP$tag_year %in% ratedoysubset$tag_year),]
# HF_badpeak_DP <- unique(percent_growth_badpeak$tag_year)
# wood_pheno_removed <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% HF_badpeak_DP,]
# wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
# count(wood_pheno_removed,sp)
# hist(wood_pheno_removed$dbh)

# subset main DF
# percent_growth_DP <- percent_growth_DP[percent_growth_DP$tag_year %in% ratedoysubset$tag_year,]

# Re-combine into single DF
percent_growth <- rbind(percent_growth_DP, percent_growth_RP)

## Clean both at once now ----
# Remove models with small or negligible growth
# ENTRIES REMOVED THIS STEP
percent_growth_gone <- subset(percent_growth, dbh_total_growth <= 0.02) # remove small growth trees
HF_smallgrowth <- unique(percent_growth_gone$tag_year)
wood_pheno_removed <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% HF_smallgrowth, ]
wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
count(wood_pheno_removed, sp)
hist(wood_pheno_removed$dbh)
# Subset the main DF
percent_growth <- subset(percent_growth, dbh_total_growth >= 0.02) # remove small growth trees


# Remove models that failed to model at least 99% of total growth using LG5.pre
percent_growth_one <- subset(percent_growth, dbh_growth_percent_cummulative >= .975) # Find tags that reach ~100% growth
unique(percent_growth_one$tag_year) # Check how many we have
# ENTRIES REMOVED IN THIS STEP
percent_growth_low <- percent_growth[!(percent_growth$tag_year %in% unique(percent_growth_one$tag_year)), ]
HF_below99 <- unique(percent_growth_low$tag_year)
wood_pheno_removed <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% HF_below99, ]
wood_pheno_removed <- subset(wood_pheno_removed, perc == .25)
count(wood_pheno_removed, sp)
hist(wood_pheno_removed$dbh)
LG5_parameter_values_removed_99_hf <- LG5_parameter_values_hf[LG5_parameter_values_hf$tag_year %in% HF_below99, ]

# Subset main DF
percent_growth <- percent_growth[(percent_growth$tag_year %in% unique(percent_growth_one$tag_year)), ] # Remove tags with <99% growth

### Messing around ###
# percent_growth$check <- 1
# percent_growth_season_length <- subset(percent_growth, dbh_growth_percent >= 0.01)
# SL <- aggregate(percent_growth_season_length$check, by = list(percent_growth_season_length$tag_year), FUN = sum)
# SL20 <- subset(SL, x <=5)
# percent_growth_SL <- percent_growth[percent_growth$tag_year %in% "D15272002",]
# percent_growth_5 <- subset(percent_growth, dbh_growth_percent >= 0.05)

percent_growth_na <- percent_growth[(is.na(percent_growth$b)), ]

percent_growth <- percent_growth[!(is.na(percent_growth$b)), ]

# percent_growth_DP_try <- percent_growth_DP %>%
#  group_by(tag_year) %>%
#  mutate(spikecheck = dbh_growth/lag(dbh_growth))
# percent_growth_two_tags <-subset(percent_growth_DP_try, spikecheck >= 7 & spikecheck<= 100)
# percent_growth_two <- percent_growth[percent_growth$tag_year %in% unique(percent_growth_two_tags$tag_year),]
# Check for patterns in removed trees
removed_tagyears <- c(HF_badpeak_DP, HF_badpeak_RP, HF_below99, HF_highgrowth_DP, HF_highgrowth_RP, HF_smallgrowth)
removed_trees <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% removed_tagyears, ]
removed_trees <- subset(removed_trees, perc == .25)
hist(removed_trees$dbh)
count(removed_trees, sp)
count(Wood_pheno_table_hf, sp)
## Remove bad models from wood_pheno_table and LG5_param DF's ----
Wood_pheno_table_hf$tag_year <- paste0(Wood_pheno_table_hf$tag, Wood_pheno_table_hf$year)
Wood_gone <- Wood_pheno_table_hf[!(Wood_pheno_table_hf$tag_year %in% unique(percent_growth$tag_year)), ]
1224 / 3
5994 / 3
408 / 1998
Wood_pheno_table_hf <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% unique(percent_growth$tag_year), ]
tot_growth <- distinct(percent_growth[, c(3, 19)], .keep_all = TRUE)
Wood_pheno_table_hf <- left_join(Wood_pheno_table_hf, tot_growth, by = "tag_year")

LG5_parameter_values_hf$tag_year <- paste0(LG5_parameter_values_hf$site_tag, LG5_parameter_values_hf$year)
LG5_parameter_values_hf <- LG5_parameter_values_hf[LG5_parameter_values_hf$tag_year %in% unique(percent_growth$tag_year), ]
# V6 - 99% V7 95% V8 put % check last V9 97.5%
write.csv(Wood_pheno_table_hf, file = "data/Wood_pheno_table_HarvardForest_V9CLEAN.csv", row.names = FALSE)
write.csv(LG5_parameter_values_hf, file = "data/LG5_parameter_values_HarvardForest_V9CLEAN.csv", row.names = FALSE)

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



fig3_hf <- ggplot(percent_growth_SL, aes(x = doy, y = dbh_growth_percent_cummulative, group = tag_year, col = wood_type)) +
  geom_line(alpha = 0.2) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Cummulative percent of total growth", title = "Cummulative percent of total (modeled) growth in diameter")
fig3_hf
