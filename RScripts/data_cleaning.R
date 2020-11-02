library(tidyverse)
library(scales)
library(tidybayes)

#SCBI ----
Wood_pheno_table_scbi <- read_csv("Data/Wood_pheno_table_V10RAW.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

LG5_parameter_values_scbi <- read_csv("Data/LG5_parameter_values_V10RAW.csv")

# Generalized 5-parameter logistic function (modified version of Sean's function)
lg5 <- function(L, K, doy_ip, r, theta, doy) {
  # For specified 5 parameters and x = doy, compute y = dbh
  dbh <- L + ((K - L) / (1 + 1/theta * exp(-(r * (doy - doy_ip) / theta)) ^ theta))
  return(dbh)
}

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
    dbh = lg5(a, b, doy_ip, r, theta, doy),
    dbh_growth = dbh - lag(dbh),
    dbh_total_growth =  b-a,
    dbh_growth_percent = dbh_growth/dbh_total_growth
  ) %>%
  filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )

percent_growth_RP <- subset(percent_growth, wood_type == "ring-porous")
maxgrowthrate <- aggregate(percent_growth_RP$dbh_growth_percent, by = list(percent_growth_RP$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate")#Maximum growth rate of each tree
high_growthday <- subset(maxgrowthrate, rate >= 0.02) #Remove trees where one day had higher than 4% of total growth
percent_growth_RP <- percent_growth_RP[!(percent_growth_RP$tag_year %in% unique(high_growthday$tag_year)),]
#maxgrowthcumul <- aggregate(percent_growth$dbh_growth_percent_cummulative, by = list(percent_growth$tag_year), FUN = max)
#maxgrowthcumul_good <- subset(maxgrowthcumul, x >= 0.99)
#percent_growth <- percent_growth[percent_growth$tag_year %in% maxgrowthcumul_good$Group.1,]
ratedoy <- percent_growth_RP[percent_growth_RP$dbh_growth_percent %in% maxgrowthrate$rate,]
ratedoysubset <- subset(ratedoy, doy >= 110 & doy <= 190) #Remove models where peak growth was in winter
percent_growth_RP <- percent_growth_RP[percent_growth_RP$tag_year %in% ratedoysubset$tag_year,]

percent_growth_DP <- subset(percent_growth, wood_type == "diffuse-porous")
maxgrowthrate <- aggregate(percent_growth_DP$dbh_growth_percent, by = list(percent_growth_DP$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate")#Maximum growth rate of each tree
high_growthday <- subset(maxgrowthrate, rate >= 0.04) #Remove trees where one day had higher than 4% of total growth
percent_growth_DP <- percent_growth_DP[!(percent_growth_DP$tag_year %in% unique(high_growthday$tag_year)),]
#maxgrowthcumul <- aggregate(percent_growth$dbh_growth_percent_cummulative, by = list(percent_growth$tag_year), FUN = max)
#maxgrowthcumul_good <- subset(maxgrowthcumul, x >= 0.99)
#percent_growth <- percent_growth[percent_growth$tag_year %in% maxgrowthcumul_good$Group.1,]
ratedoy <- percent_growth_DP[percent_growth_DP$dbh_growth_percent %in% maxgrowthrate$rate,]
ratedoysubset <- subset(ratedoy, doy >= 125 & doy <= 220) #Remove models where peak growth was in winter
percent_growth_DP <- percent_growth_DP[percent_growth_DP$tag_year %in% ratedoysubset$tag_year,]

percent_growth <- rbind(percent_growth_DP, percent_growth_RP)

percent_growth_one <- subset(percent_growth, dbh_growth_percent_cummulative >= .99) #Find tags that reach ~100% growth
unique(percent_growth_one$tag_year)
percent_growth <- percent_growth[(percent_growth$tag_year %in% unique(percent_growth_one$tag_year)),] #Remove tags with <99% growth
percent_growth_gone <- subset(percent_growth, dbh_total_growth >= 0.05) #remove small growth trees

percent_growth_vari <- percent_growth[percent_growth$dbh_growth_percent_cummulative >=1.1,] #Remove trees with > 100% growth
#percent_growth$Round <- round(percent_growth$dbh_growth_percent_cummulative, digits = -2)
#percent_growth_low <-subset(percent_growth, dbh_growth_percent_cummulative >= 0.99)
#unique(percent_growth_low$tag_year)
#pg_new <- unique(percent_growth$tag_year)
percent_growth <- percent_growth[!(percent_growth$tag_year %in% unique(percent_growth_vari$tag_year)),]
#percent_growth <- percent_growth[percent_growth$tag_year %in% unique(percent_growth_low$tag_year),]


#maxgrowthrate <- aggregate(percent_growth$dbh_growth_percent, by = list(percent_growth$tag_year), FUN = max)
#names(maxgrowthrate) <- c("tag_year", "rate")#Maximum growth rate of each tree
#high_growthday <- subset(maxgrowthrate, rate >= 0.04) #Remove trees where one day had higher than 4% of total growth
#percent_growth <- percent_growth[!(percent_growth$tag_year %in% unique(high_growthday$tag_year)),]
#maxgrowthcumul <- aggregate(percent_growth$dbh_growth_percent_cummulative, by = list(percent_growth$tag_year), FUN = max)
#maxgrowthcumul_good <- subset(maxgrowthcumul, x >= 0.99)
#percent_growth <- percent_growth[percent_growth$tag_year %in% maxgrowthcumul_good$Group.1,]
#ratedoy <- percent_growth[percent_growth$dbh_growth_percent %in% maxgrowthrate$rate,]
#ratedoysubset <- subset(ratedoy, doy >= 125 & doy <= 220) #Remove models where peak growth was in winter
#percent_growth <- percent_growth[percent_growth$tag_year %in% ratedoysubset$tag_year,]

#outliers <- subset(percent_growth, dbh_growth_percent_cummulative >= 1.01)
#greaterthan100 <- unique(outliers$tag_year)

#percent_growth <- percent_growth[!(percent_growth$tag_year %in% greaterthan100),  ]
Wood_pheno_table_scbi <-  Wood_pheno_table_scbi %>%
  separate(tag, into = c("tag", "stem"), sep = "_") %>%
  mutate(
    tag = as.numeric(tag),
    tag_year = str_c(tag, year)
  )
Wood_pheno_table_scbi <- Wood_pheno_table_scbi[Wood_pheno_table_scbi$tag_year %in% unique(percent_growth$tag_year),]
LG5_parameter_values_scbi$tag_year <- paste0(LG5_parameter_values_scbi$tag, LG5_parameter_values_scbi$year)
LG5_parameter_values_scbi <- LG5_parameter_values_scbi[LG5_parameter_values_scbi$tag_year %in% unique(percent_growth$tag_year),]

write.csv(Wood_pheno_table_scbi, file = "data/Wood_pheno_table_V10CLEAN.csv", row.names = FALSE)
write.csv(LG5_parameter_values_scbi, file = "data/LG5_parameter_values_V10CLEAN.csv", row.names = FALSE)


#LG5_parameter_values_scbi$tot_lg5 <- lg5(LG5_parameter_values_scbi$L, LG5_parameter_values_scbi$K, LG5_parameter_values_scbi$doy_ip, LG5_parameter_values_scbi$r, LG5_parameter_values_scbi$theta, 365)-lg5(LG5_parameter_values_scbi$L, LG5_parameter_values_scbi$K, LG5_parameter_values_scbi$doy_ip, LG5_parameter_values_scbi$r, LG5_parameter_values_scbi$theta, 1)

#ratedoy_dropped <- ratedoy[!(ratedoy$tag_year %in% ratedoysubset$tag_year),]
#percent_growth_dropped <- percent_growth[percent_growth$tag_year %in% ratedoy_dropped$tag_year,]

rel_growth_scbi <-ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent, group = tag_year)) +
  coord_cartesian(ylim = c(0, 0.05)) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Percent of total growth",
       title = "Percent of total (modeled) growth in diameter",
       subtitle = "Each curve represents one tag-year") +
  theme_bw() +
  geom_line(alpha = 0.2, aes(col = wood_type)) +
  scale_colour_viridis_d("Wood Type", end = 2/3)
rel_growth_scbi
#fig_width <- 7
#ggsave(filename = "doc/manuscript/tables_figures/rel_growth.png",
#       plot = rel_growth,
#       width = fig_width, height = fig_width / 2)



fig3_scbi <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent_cummulative, group = tag_year, col = wood_type)) +
  geom_line(alpha = 0.2) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Cummulative percent of total growth", title = "Cummulative percent of total (modeled) growth in diameter")
fig3_scbi

#fig_width <- 6
#ggsave(filename = "doc/manuscript/tables_figures/fig3.png", plot = fig3, width = fig_width, height = fig_width / 1.52)

#Harvard Forest ----
Wood_pheno_table_hf <- read_csv("Data/Wood_pheno_table_HarvardForest_V4.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

Wood_pheno_table_hf$site_tag <- Wood_pheno_table_hf$tag
Wood_pheno_table_hf$site <- substr(Wood_pheno_table_hf$tag,1, 2)
Wood_pheno_table_hf$realtag <- substr(Wood_pheno_table_hf$site_tag,3, nchar(as.character(Wood_pheno_table_hf$site_tag)))

LG5_parameter_values_hf <- read_csv("Data/LG5_parameter_values_HarvardForest_V3.csv")
LG5_parameter_values_hf$site_tag <- paste0(LG5_parameter_values_hf$plot, LG5_parameter_values_hf$tag)

# Generalized 5-parameter logistic function (modified version of Sean's function)
lg5 <- function(L, K, doy_ip, r, theta, doy) {
  # For specified 5 parameters and x = doy, compute y = dbh
  dbh <- L + ((K - L) / (1 + 1/theta * exp(-(r * (doy - doy_ip) / theta)) ^ theta))
  return(dbh)
}
#ML VALUE SUBSET
#LG5_parameter_values <- subset(LG5_parameter_values_scbi, ML_value >= 100)

percent_growth <- Wood_pheno_table_hf %>%
  #separate(tag, into = c("tag", "stem"), sep = "_") %>%
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
    dbh_total_growth = K-L,
    dbh_growth_percent = dbh_growth/dbh_total_growth
  ) %>%
  filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )

percent_growth_RP <- subset(percent_growth, wood_type == "ring-porous")
maxgrowthrate <- aggregate(percent_growth_RP$dbh_growth_percent, by = list(percent_growth_RP$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate")#Maximum growth rate of each tree
high_growthday <- subset(maxgrowthrate, rate >= 0.02) #Remove trees where one day had higher than 4% of total growth
percent_growth_RP <- percent_growth_RP[!(percent_growth_RP$tag_year %in% unique(high_growthday$tag_year)),]
#maxgrowthcumul <- aggregate(percent_growth$dbh_growth_percent_cummulative, by = list(percent_growth$tag_year), FUN = max)
#maxgrowthcumul_good <- subset(maxgrowthcumul, x >= 0.99)
#percent_growth <- percent_growth[percent_growth$tag_year %in% maxgrowthcumul_good$Group.1,]
ratedoy <- percent_growth_RP[percent_growth_RP$dbh_growth_percent %in% maxgrowthrate$rate,]
ratedoysubset <- subset(ratedoy, doy >= 110 & doy <= 190) #Remove models where peak growth was in winter
percent_growth_RP <- percent_growth_RP[percent_growth_RP$tag_year %in% ratedoysubset$tag_year,]

percent_growth_DP <- subset(percent_growth, wood_type == "diffuse-porous")
maxgrowthrate <- aggregate(percent_growth_DP$dbh_growth_percent, by = list(percent_growth_DP$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate")#Maximum growth rate of each tree
high_growthday <- subset(maxgrowthrate, rate >= 0.04) #Remove trees where one day had higher than 4% of total growth
percent_growth_DP <- percent_growth_DP[!(percent_growth_DP$tag_year %in% unique(high_growthday$tag_year)),]
#maxgrowthcumul <- aggregate(percent_growth$dbh_growth_percent_cummulative, by = list(percent_growth$tag_year), FUN = max)
#maxgrowthcumul_good <- subset(maxgrowthcumul, x >= 0.99)
#percent_growth <- percent_growth[percent_growth$tag_year %in% maxgrowthcumul_good$Group.1,]
ratedoy <- percent_growth_DP[percent_growth_DP$dbh_growth_percent %in% maxgrowthrate$rate,]
ratedoysubset <- subset(ratedoy, doy >= 125 & doy <= 220) #Remove models where peak growth was in winter
percent_growth_DP <- percent_growth_DP[percent_growth_DP$tag_year %in% ratedoysubset$tag_year,]

percent_growth <- rbind(percent_growth_DP, percent_growth_RP)

percent_growth_one <- subset(percent_growth, dbh_growth_percent_cummulative >= .99) #Find tags that reach ~100% growth
unique(percent_growth_one$tag_year)
percent_growth <- percent_growth[(percent_growth$tag_year %in% unique(percent_growth_one$tag_year)),] #Remove tags with <99% growth
percent_growth <- subset(percent_growth, dbh_total_growth >= 0.05) #remove small growth trees
percent_growth_vari <- percent_growth[percent_growth$dbh_growth_percent_cummulative >=1.1,] #Remove trees with > 100% growth
#percent_growth$Round <- round(percent_growth$dbh_growth_percent_cummulative, digits = -2)
#percent_growth_low <-subset(percent_growth, dbh_growth_percent_cummulative >= 0.99)
#unique(percent_growth_low$tag_year)
#pg_new <- unique(percent_growth$tag_year)
percent_growth <- percent_growth[!(percent_growth$tag_year %in% unique(percent_growth_vari$tag_year)),]
#percent_growth <- percent_growth[percent_growth$tag_year %in% unique(percent_growth_low$tag_year),]


#maxgrowthrate <- aggregat
######################
#percent_growth <- subset(percent_growth, dbh_total_growth >= 0.05)
#percent_growth_vari <- percent_growth[percent_growth$dbh_growth_percent_cummulative >=1.1,]
#percent_growth$Round <- round(percent_growth$dbh_growth_percent_cummulative, digits = -2)
#percent_growth_low <-subset(percent_growth, dbh_growth_percent_cummulative >= 0.99)
#unique(percent_growth_low$tag_year)
#pg_new <- unique(percent_growth$tag_year)
#percent_growth <- percent_growth[!(percent_growth$tag_year %in% unique(percent_growth_vari$tag_year)),]
#percent_growth <- percent_growth[percent_growth$tag_year %in% unique(percent_growth_low$tag_year),]
#mean(LG5_parameter_values$theta)
#sd(LG5_parameter_values$theta)
#mean(LG5_parameter_values$theta)+(2*sd(LG5_parameter_values$theta))

#highgrowthdays <- subset(percent_growth, dbh_growth_percent >= .05)
#highgrowthtagyears <- unique(highgrowthdays$tag_year)
#percent_growth <- percent_growth[!(percent_growth$tag_year %in% highgrowthtagyears),]

#maxgrowthrate <- aggregate(percent_growth$dbh_growth_percent, by = list(percent_growth$tag_year), FUN = max)
#names(maxgrowthrate) <- c("tag_year", "rate")
#ratedoy <- percent_growth[percent_growth$dbh_growth_percent %in% maxgrowthrate$rate,]
#aggregate(ratedoy$doy, by = list(ratedoy$wood_type), FUN = mean)
#163-(14*3)
#176+(14*3)
#ratedoysubset <- subset(ratedoy, doy >= 125 & doy <= 220)
#percent_growth <- percent_growth[percent_growth$tag_year %in% ratedoysubset$tag_year,]

#unique(percent_growth$tag_year)

#percent_growth <- subset(percent_growth, dbh_total_growth >= 0.03)
Wood_pheno_table_hf$tag_year <- paste0(Wood_pheno_table_hf$tag, Wood_pheno_table_hf$year)
Wood_pheno_table_hf <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag_year %in% unique(percent_growth$tag_year),]
LG5_parameter_values_hf$tag_year <- paste0(LG5_parameter_values_hf$site_tag, LG5_parameter_values_hf$year)
LG5_parameter_values_hf <- LG5_parameter_values_hf[LG5_parameter_values_hf$tag_year %in% unique(percent_growth$tag_year),]

write.csv(Wood_pheno_table_hf, file = "data/Wood_pheno_table_HarvardForest_V5CLEAN.csv", row.names = FALSE)
write.csv(LG5_parameter_values_hf, file = "data/LG5_parameter_values_HarvardForest_V5CLEAN.csv", row.names = FALSE)

rel_growth_hf <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent, group = tag_year)) +
  coord_cartesian(ylim = c(0, 0.05)) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Percent of total growth",
       title = "Percent of total (modeled) growth in diameter",
       subtitle = "Each curve represents one tag-year") +
  theme_bw() +
  geom_line(alpha = 0.2, aes(col = wood_type)) +
  scale_colour_viridis_d("Wood Type", end = 2/3)
rel_growth_hf
#fig_width <- 7
#ggsave(filename = "doc/manuscript/tables_figures/rel_growth.png",
#       plot = rel_growth,
#       width = fig_width, height = fig_width / 2)



fig3_hf <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent_cummulative, group = tag_year, col = wood_type)) +
  geom_line(alpha = 0.2) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Cummulative percent of total growth", title = "Cummulative percent of total (modeled) growth in diameter")
fig3_hf

