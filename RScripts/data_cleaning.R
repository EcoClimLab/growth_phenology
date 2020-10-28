library(tidyverse)
library(scales)
library(tidybayes)

#SCBI ----
Wood_pheno_table_scbi <- read_csv("Data/Wood_pheno_table_V8RAW.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

LG5_parameter_values_scbi <- read_csv("Data/LG5_parameter_values_V8RAW.csv")

# Generalized 5-parameter logistic function (modified version of Sean's function)
lg5 <- function(L, K, doy_ip, r, theta, doy) {
  # For specified 5 parameters and x = doy, compute y = dbh
  dbh <- L + ((K - L) / (1 + 1/theta * exp(-(r * (doy - doy_ip) / theta)) ^ theta))
  return(dbh)
}

#LG5_parameter_values <- subset(LG5_parameter_values_scbi, ML_value >= 100)

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
    dbh_total_growth =  lg5(L, K, doy_ip, r, theta, 365)- lg5(L, K, doy_ip, r, theta, 1),
    dbh_growth_percent = dbh_growth/dbh_total_growth
  ) %>%
  filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )

maxgrowthrate <- aggregate(percent_growth$dbh_growth_percent, by = list(percent_growth$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate")

ratedoy <- percent_growth[percent_growth$dbh_growth_percent %in% maxgrowthrate$rate,]
ratedoysubset <- subset(ratedoy, doy >= 110 & doy <= 250)
percent_growth <- percent_growth[percent_growth$tag_year %in% ratedoysubset$tag_year,]

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

write.csv(Wood_pheno_table_scbi, file = "data/Wood_pheno_table_V8CLEAN.csv", row.names = FALSE)
write.csv(LG5_parameter_values_scbi, file = "data/LG5_parameter_values_V8CLEAN.csv", row.names = FALSE)



rel_growth_scbi <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent, group = tag_year)) +
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
Wood_pheno_table_hf <- read_csv("Data/Wood_pheno_table_HarvardForest_V3RAW.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

Wood_pheno_table_hf$site_tag <- substr(Wood_pheno_table_hf$tag,1, nchar(as.character(Wood_pheno_table_hf$tag))-4)
Wood_pheno_table_hf$site <- substr(Wood_pheno_table_hf$tag,1, 2) #use this to split cores up into two columns, ID and letter, then use unique id's to pick one of each core?
Wood_pheno_table_hf$realtag <- substr(Wood_pheno_table_hf$site_tag,3, nchar(as.character(Wood_pheno_table_hf$site_tag))) #use this to split cores up into two columns, ID and letter, then use unique id's to pick one of each core?

LG5_parameter_values_hf <- read_csv("Data/LG5_parameter_values_HarvardForest_V3RAW.csv")
LG5_parameter_values_hf$site_tag <- paste0(LG5_parameter_values_hf$plot, LG5_parameter_values_hf$tag)

# Generalized 5-parameter logistic function (modified version of Sean's function)
lg5 <- function(L, K, doy_ip, r, theta, doy) {
  # For specified 5 parameters and x = doy, compute y = dbh
  dbh <- L + ((K - L) / (1 + 1/theta * exp(-(r * (doy - doy_ip) / theta)) ^ theta))
  return(dbh)
}

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
    dbh_total_growth = lg5(L, K, doy_ip, r, theta, 365)- lg5(L, K, doy_ip, r, theta, 1),
    dbh_growth_percent = dbh_growth/dbh_total_growth
  ) %>%
  filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )


highgrowthdays <- subset(percent_growth, dbh_growth_percent >= .06)
highgrowthtagyears <- unique(highgrowthdays$tag_year)
percent_growth <- percent_growth[!(percent_growth$tag_year %in% highgrowthtagyears),]

maxgrowthrate <- aggregate(percent_growth$dbh_growth_percent, by = list(percent_growth$tag_year), FUN = max)
names(maxgrowthrate) <- c("tag_year", "rate")
ratedoy <- percent_growth[percent_growth$dbh_growth_percent %in% maxgrowthrate$rate,]
ratedoysubset <- subset(ratedoy, doy >= 110 & doy <= 250)
percent_growth <- percent_growth[percent_growth$tag_year %in% ratedoysubset$tag_year,]

percent_growth <- subset(percent_growth, dbh_total_growth >= 0.03)

Wood_pheno_table_hf <- Wood_pheno_table_hf[Wood_pheno_table_hf$tag %in% unique(percent_growth$tag_year),]
LG5_parameter_values_hf$tag_year <- paste0(LG5_parameter_values_hf$site_tag, LG5_parameter_values_hf$year)
LG5_parameter_values_hf <- LG5_parameter_values_hf[LG5_parameter_values_hf$tag_year %in% unique(percent_growth$tag_year),]

write.csv(Wood_pheno_table_hf, file = "data/Wood_pheno_table_HarvardForest_V3CLEAN.csv", row.names = FALSE)
write.csv(LG5_parameter_values_hf, file = "data/LG5_parameter_values_HarvardForest_V3CLEAN.csv", row.names = FALSE)

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


