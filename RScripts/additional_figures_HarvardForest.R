library(tidyverse)
library(scales)
library(tidybayes)

# Figure numbers from: https://github.com/SCBI-ForestGEO/growth_phenology/issues/13
unique(Wood_pheno_table_HarvardForest_V2$sp)


# Fig2 & Fig3: Percent modeled growth and cummulative percent modeled growth ---
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_HarvardForest_V2.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

Wood_pheno_table$site_tag <- substr(Wood_pheno_table$tag,1, nchar(as.character(Wood_pheno_table$tag))-4) #use this to split cores up into two columns, ID and letter, then use unique id's to pick one of each core?
Wood_pheno_table$site <- substr(Wood_pheno_table$tag,1, 2) #use this to split cores up into two columns, ID and letter, then use unique id's to pick one of each core?
Wood_pheno_table$realtag <- substr(Wood_pheno_table$site_tag,3, nchar(as.character(Wood_pheno_table$site_tag))) #use this to split cores up into two columns, ID and letter, then use unique id's to pick one of each core?

# As generated in Rscripts/SCBI_wood_phenology.R
LG5_parameter_values <- read_csv("Data/LG5_parameter_values_HarvardForest_V2.csv")
LG5_parameter_values$site_tag <- paste0(LG5_parameter_values$plot, LG5_parameter_values$tag)
# Generalized 5-parameter logistic function (modified version of Sean's function)
lg5 <- function(L, K, doy_ip, r, theta, doy) {
# For specified 5 parameters and x = doy, compute y = dbh
  dbh <- L + ((K - L) / (1 + 1/theta * exp(-(r * (doy - doy_ip) / theta)) ^ theta))
  return(dbh)
}

percent_growth <- Wood_pheno_table %>%
  #separate(tag, into = c("tag", "stem"), sep = "_") %>%
  mutate(
    site_tag = site_tag,
    tag_year = str_c(site_tag, year)
  ) %>%
  select(site_tag, year, tag_year, wood_type) %>%
  distinct() %>%
  left_join(LG5_parameter_values, by = c("site_tag", "year")) %>%
  group_by(site_tag, year) %>%
  mutate(doy = list(seq(from = 1, to = 365, by = 1))) %>%
  unnest_longer(doy) %>%
  group_by(tag_year) %>%
  mutate(
    dbh = lg5(L, K, doy_ip, r, theta, doy),
    dbh_growth = dbh - lag(dbh),
    dbh_total_growth = b - a,
    dbh_growth_percent = dbh_growth/dbh_total_growth
  ) %>%
  filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )

percent_growth <- subset(percent_growth, dbh_total_growth >= .1)
percent_growth_vari <- percent_growth[percent_growth$dbh_growth_percent_cummulative >=1.1,]
#pg_new <- unique(percent_growth$tag_year)
percent_growth <- percent_growth[!(percent_growth$tag_year %in% percent_growth_vari$tag_year),]
#mean(LG5_parameter_values$theta)
#sd(LG5_parameter_values$theta)
#mean(LG5_parameter_values$theta)+(2*sd(LG5_parameter_values$theta))

#LG5_parameter_values_subset <- subset(LG5_parameter_values, theta <= mean(LG5_parameter_values$theta)+sd(LG5_parameter_values$theta) & theta >= mean(LG5_parameter_values$theta)-sd(LG5_parameter_values$theta))
Wood_pheno_table <- Wood_pheno_table[Wood_pheno_table$tag %in% percent_growth$tag_year,]
write.csv(Wood_pheno_table, file = "wood_pheno_table_HFtemp.csv", row.names = FALSE)
rel_growth <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent, group = tag_year)) +
  coord_cartesian(ylim = c(0, 0.05)) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Percent of total growth",
       title = "Percent of total (modeled) growth in diameter",
       subtitle = "Each curve represents one tag-year") +
  theme_bw() +
  geom_line(alpha = 0.2, aes(col = wood_type)) +
  scale_colour_viridis_d("Wood Type", end = 2/3)
rel_growth

fig_width <- 7
ggsave(filename = "doc/manuscript/tables_figures/rel_growth_HF.png",
       plot = rel_growth,
       width = fig_width, height = fig_width / 2)



fig3 <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent_cummulative, group = tag_year, col = wood_type)) +
  geom_line(alpha = 0.2) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Cummulative percent of total growth", title = "Cummulative percent of total (modeled) growth in diameter")
fig3

fig_width <- 6
ggsave(filename = "doc/manuscript/tables_figures/fig3_HF_V1.png", plot = fig3, width = fig_width, height = fig_width / 1.52)



# fig3b <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent_cummulative, group = wood_type)) +
#   stat_lineribbon(.width = c(.95, 0.9),  color = "#08519C") +
#   facet_wrap(~wood_type, ncol = 1) +
#   scale_y_continuous(labels = percent) +
#   scale_fill_brewer() +
#   labs(x = "DOY", y = "Cummulative percent of total growth", title = "Cummulative percent of total (modeled) growth in diameter", subtitle = "Each curve represents one tag-year")
# fig3b

#Figure D'Orangeville figure 4
warmestRP <- subset(Wood_pheno_table, year == 1999 & wood_type == "ring-porous")
warmestDP <- subset(Wood_pheno_table, year == 1999 & wood_type == "diffuse-porous")
warmest <- rbind(warmestRP, warmestDP)
coldestRP <- subset(Wood_pheno_table, year == 2003 & wood_type == "ring-porous")
coldestDP <- subset(Wood_pheno_table, year == 2003 & wood_type == "diffuse-porous")
coldest <- rbind(coldestDP, coldestRP)
aggregates <- aggregate(Wood_pheno_table$DOY, by = list(Wood_pheno_table$wood_type, Wood_pheno_table$perc), FUN = mean)
aggregates$temp_type <- "Average"
aggregates_warm <- aggregate(warmest$DOY, by = list(warmest$wood_type, warmest$perc), FUN = mean)
names(aggregates_warm) <- c("Group.1", "Group.2", "x")
aggregates_warm$temp_type <- "Warmest Year"
aggregates_cold <- aggregate(coldest$DOY, by = list(coldest$wood_type, coldest$perc), FUN = mean)
names(aggregates_cold) <- c("Group.1", "Group.2", "x")
aggregates_cold$temp_type <- "Coldest Year"

aggregates <- rbind(aggregates, aggregates_cold, aggregates_warm)
aggregates <- aggregates %>% mutate(temp_type = factor(temp_type, levels = c("Warmest Year", "Average", "Coldest Year")))

#aggregates$Group.2 <- ifelse(aggregates$Group.2 == .25, 25, ifelse(aggregates$Group.2 == .50, 50, ifelse(aggregates$Group.2 == .75, 75, 0)))

#aggregates <- left_join(aggregates, aggregates_cold, by = c("Group.1", "Group.2"))
#aggregates <- left_join(aggregates, aggregates_warm, by = c("Group.1", "Group.2"))
#names(aggregates) <- c("Group.1", "Group.2","x","cold", "warm")
#names(aggregates) <- c("Wood type", "Growth vari", "DOY")
doytiming <- ggplot(aggregates, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, temp_type), color = Group.1, linetype = temp_type))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "Harvard Forest Intraannual Growth Timing", color = "Wood Type")+
  scale_colour_viridis_d(end = 2/3)


fig_width <- 7
ggsave(filename = "doc/manuscript/tables_figures/DOYtiming_HF_PRELIM.png",
       plot = doytiming,
       width = fig_width, height = fig_width / 2)

####Tags per Year table ----
all_stems$tag_year <- paste(all_stems$tag, all_stems$year, sep = "_")
unique <- unique(all_stems$tag_year)
unique <- data.frame(unique)
unique$year <- substr(unique$unique,nchar(as.character(unique$unique))-3, nchar(as.character(unique$unique))) #use this to split cores up into two columns, ID and letter, then use unique id's to pick one of each core?
tags_per_year <- count(unique$year)
names(tags_per_year) <- c("Year", "SCBI")

all_stems$tag_year <- paste(all_stems$plot, all_stems$tag, all_stems$year, sep = "_")
unique <- unique(all_stems$tag_year)
unique <- data.frame(unique)
unique$year <- substr(unique$unique,nchar(as.character(unique$unique))-3, nchar(as.character(unique$unique))) #use this to split cores up into two columns, ID and letter, then use unique id's to pick one of each core?
tags_per_year_HF <- count(unique$year)
names(tags_per_year_HF) <- c("Year", "Harvard Forest")

tags <- merge(tags_per_year,tags_per_year_HF, by = "Year", all = TRUE)
tags$Year <- as.character(tags$Year)
tags$Year <- as.numeric(tags$Year)
tags <- tags[order(tags$Year),-4]

write.csv(tags, file = "Tags_per_year.csv", row.names = FALSE)

#Species distibution of bands
stems_unclean <- read.csv("data/Wood_pheno_table_HarvardForest_V2.csv")
stems_clean <- read_csv("data/wood_pheno_table_HFtemp.csv")
stems_clean <- subset(stems_clean, perc == .25)
stems_unclean <- subset(stems_unclean, perc == .25 & wood_type != "other")

clean <- count(stems_clean$sp)
unclean <-count(stems_unclean$sp)

clean$x
Species <- c("Striped Maple", "Red Maple", "Yellow Birch", "Black Birch", "White Birch", "Grey Birch",
             "American Beech", "White Ash", "Black Cherry", "Red Oak", "Black Oak")
clean$Species <- Species
names(clean) <- c("SP code", "After Cleaning", "Species")
clean <- clean[,c(2,3)]
unclean$Species <- Species
names(unclean) <- c("SP code", "Before Cleaning", "Species")
unclean <- unclean[,c(2,3)]

merged <- merge(clean, unclean, by = "Species")
merged <- merged[,c(1,3,2)]

merged$Wood_Structure <-c("Diffuse-porous","Diffuse-porous","Diffuse-porous","Ring-Porous","Diffuse-porous","Diffuse-porous","Ring-Porous","Diffuse-porous" ,"Ring-Porous","Diffuse-porous","Diffuse-porous")
merged <- merged[order(merged$Wood_Structure),]

write.csv(merged, file = "HF_bands_bySP.csv", row.names = FALSE)

###SCBI
stems_unclean <- read.csv("data/Wood_pheno_table_V6.csv")
stems_clean <- read.csv("data/Wood_pheno_table_V7.csv")
stems_clean <- subset(stems_clean, perc == .25)
stems_unclean <- subset(stems_unclean, perc == .25 & wood_type != "other")

clean <- count(stems_clean$sp)
unclean <-count(stems_unclean$sp)

Species <- c("American Beech", "Tulip Poplar", "White Oak", "Red Oak")
clean$Species <- Species
names(clean) <- c("SP code", "After Cleaning", "Species")
clean <- clean[,c(2,3)]
unclean$Species <- Species
names(unclean) <- c("SP code", "Before Cleaning", "Species")
unclean <- unclean[,c(2,3)]

merged <- merge(clean, unclean, by = "Species")
merged <- merged[,c(1,3,2)]

merged$Wood_Structure <-c("Diffuse-porous","Ring-Porous","Diffuse-porous","Ring-Porous")
merged <- merged[order(merged$Wood_Structure),]

write.csv(merged, file = "SCBI_bands_bySP.csv", row.names = FALSE)
