# Figure numbers from: https://github.com/SCBI-ForestGEO/growth_phenology/issues/13
library(tidyverse)
library(scales)
library(ggrepel)
library(tidybayes)
library(ggnewscale)
library(patchwork)
library(lubridate)



# Figure 1 v2: Schematic illustrating parameters and general sense of results ----
## Setup ----
# DOY:
start_doy <- 1
end_doy <- 365

# Critical temperature window:
window_open <- 91
window_close <- 135

# Function for logistic growth model written by Sean. Supposed to be in RDendrom
# package https://github.com/seanmcm/RDendrom, but function does not seem to be included
lg5.pred <- function(params, doy) {
  L <- params[1] # min(dbh, na.rm = T)
  K <- params[2]
  doy.ip <- params[3]
  r <- params[4]
  theta <- params[5]
  dbh <- vector(length = length(doy))
  dbh <- L + ((K - L) / (1 + 1 / theta * exp(-(r * (doy - doy.ip) / theta))^theta))
  return(dbh)
}


## 1. Get LG5 parameter values ----
# Identify cold and hot aprils
read_csv("climate data/HF_weatherdata.csv") %>%
  dplyr::filter(month == 4) %>%
  group_by(year) %>%
  summarize(TMAX = mean(airtmax, na.rm = TRUE)) %>%
  arrange(desc(TMAX))

# Real red/hot curve values:
HF_hot_LG5_values <- read_csv("Data/LG5_parameter_values_HarvardForest_CLEAN.csv") %>%
  dplyr::filter(year == 2002) %>%
  group_by(tag_year) %>%
  summarize(L = mean(L), K = mean(K), doy_ip = mean(doy_ip), r = mean(r), theta = mean(theta)) %>%
  summarize(L = mean(L), K = mean(K), doy_ip = mean(doy_ip), r = mean(r), theta = mean(theta))

HF_L_hot <- HF_hot_LG5_values$L
HF_K_hot <- HF_hot_LG5_values$K
HF_doy.ip_hot <- HF_hot_LG5_values$doy_ip
HF_r_hot <- HF_hot_LG5_values$r
HF_theta_hot <- HF_hot_LG5_values$theta
HF_params_hot <- c(HF_L_hot, HF_K_hot, HF_doy.ip_hot, HF_r_hot, HF_theta_hot)
HF_total_growth_hot <- HF_K_hot - HF_L_hot

# Real blue/cold curve values:
HF_cold_LG5_values <- read_csv("Data/LG5_parameter_values_HarvardForest_CLEAN.csv") %>%
  dplyr::filter(year == 2003) %>%
  group_by(tag_year) %>%
  summarize(L = mean(L), K = mean(K), doy_ip = mean(doy_ip), r = mean(r), theta = mean(theta)) %>%
  summarize(L = mean(L), K = mean(K), doy_ip = mean(doy_ip), r = mean(r), theta = mean(theta))

HF_L_cold <- HF_cold_LG5_values$L
HF_K_cold <- HF_cold_LG5_values$K
HF_doy.ip_cold <- HF_cold_LG5_values$doy_ip
HF_r_cold <- HF_cold_LG5_values$r
HF_theta_cold <- HF_cold_LG5_values$theta
HF_params_cold <- c(HF_L_cold, HF_K_cold, HF_doy.ip_cold, HF_r_cold, HF_theta_cold)
HF_total_growth_cold <- HF_K_cold - HF_L_cold



## 2. Compute all values of LG5 growth curve ----
### Compute HF values ----
HF_true_values_hot <-
  tibble(
    doy = seq(from = start_doy, to = end_doy, by = 0.01),
    diameter = lg5.pred(HF_params_hot, doy)
  ) %>%
  mutate(
    perc = (diameter - HF_L_hot)/HF_total_growth_hot
  )

HF_true_values_cold <-
  tibble(
    doy = seq(from = start_doy, to = end_doy, by = 0.01),
    diameter = lg5.pred(HF_params_cold, doy)
  ) %>%
  mutate(
    perc = (diameter - HF_L_cold)/HF_total_growth_cold
  )

HF_true_values <-
  bind_rows(
    HF_true_values_hot %>% mutate(year = "Hot"),
    HF_true_values_cold %>% mutate(year = "Cold")
  )


## 3. Compute DOY for 25%/50%/75% and g_max ----
### Compute HF values ----
HF_doy_diameter_quartile_hot <- bind_rows(
  tibble(doy = 1, diameter = HF_L_hot),
  HF_true_values_hot %>% dplyr::filter(diameter >= (HF_L_hot + HF_total_growth_hot * .25)) %>% slice(1),
  HF_true_values_hot %>% dplyr::filter(diameter >= (HF_L_hot + HF_total_growth_hot * .5)) %>% slice(1),
  HF_true_values_hot %>% dplyr::filter(diameter >= (HF_L_hot + HF_total_growth_hot * .75)) %>% slice(1),
  tibble(doy = 365, diameter = HF_K_hot)
) %>%
  mutate(
    growth = c(0, 0.25, 0.5, 0.75, 1),
    label = c("0%", "25%", "50%", "75%", "100%"),
    label = factor(label, levels = c("0%", "25%", "50%", "75%", "100%")),
    year = "Hot"
  ) %>%
  select(-perc) %>%
  rename(perc = growth)

HF_doy_diameter_quartile_cold <- bind_rows(
  tibble(doy = 1, diameter = HF_L_cold),
  HF_true_values_cold %>% dplyr::filter(diameter >= (HF_L_cold + HF_total_growth_cold * .25)) %>% slice(1),
  HF_true_values_cold %>% dplyr::filter(diameter >= (HF_L_cold + HF_total_growth_cold * .5)) %>% slice(1),
  HF_true_values_cold %>% dplyr::filter(diameter >= (HF_L_cold + HF_total_growth_cold * .75)) %>% slice(1),
  tibble(doy = 365, diameter = HF_K_cold)
) %>%
  mutate(
    growth = c(0, 0.25, 0.5, 0.75, 1),
    label = c("0%", "25%", "50%", "75%", "100%"),
    label = factor(label, levels = c("0%", "25%", "50%", "75%", "100%")),
    year = "Cold"
  ) %>%
  select(-perc) %>%
  rename(perc = growth)

HF_doy_diameter_quartile <- bind_rows(
  HF_doy_diameter_quartile_hot %>% mutate(year = "Hot"),
  HF_doy_diameter_quartile_cold %>% mutate(year = "Cold")
) %>%
  mutate(year = as.factor(year))

# Semi-synthetic DOY_g_max dates
HF_doy_max_rate_offset <- 2
HF_doy_max_rate <- HF_doy_diameter_quartile %>%
  filter(perc == 0.5) %>%
  mutate(doy = doy + HF_doy_max_rate_offset) %>%
  mutate(year = as.factor(year))

HF_doy_max_rate <- bind_rows(
  HF_true_values_cold %>% mutate(diff = abs(perc-0.53)) %>% arrange(diff) %>% slice(1) %>% mutate(year = "Cold"),
  HF_true_values_hot %>% mutate(diff = abs(perc-0.53)) %>% arrange(diff) %>% slice(1) %>% mutate(year = "Hot")
) %>%
  mutate(year = as.factor(year))




## 4. Identify significant horizontal shifts for 25%/50%/75% and g_max ----
### Identify HF values ----
# 25%/50%/75%
HF_significant_perc <-
  inner_join(
    HF_doy_diameter_quartile_hot %>%
      dplyr::filter(!label %in% c("0%", "100%")) %>%
      select(doy_hot = doy, perc),
    HF_doy_diameter_quartile_cold %>%
      dplyr::filter(!label %in% c("0%", "100%")) %>%
      select(doy_cold = doy, perc),
    by = "perc"
  ) %>%
  mutate(
    significant = c(TRUE, TRUE, TRUE)
  )

# Add g_max
HF_significant_perc <- bind_rows(
  HF_significant_perc,
  tibble(
    doy_hot = HF_doy_max_rate$doy[HF_doy_max_rate$year == "Hot"],
    perc = 0.53,
    doy_cold = HF_doy_max_rate$doy[HF_doy_max_rate$year == "Cold"],
    significant = TRUE
  )
)




## 5. Output figure ----
geom.text.size <- 4
theme.size <- (14/5) * geom.text.size
hot_color <- "#F8766D"
cold_color <- "#00BFC4"
hot_color <- "red"
cold_color <- "dodgerblue3"
plot_xlim <- c(window_open, 220)

# Significant vertical shift for delta DBH
HF_vertical_shift_doy <- plot_xlim[2] + 4
HF_significant_vertical <- HF_true_values %>%
  mutate(diff = abs(doy - HF_vertical_shift_doy)) %>%
  arrange(diff) %>%
  slice(1:2) %>%
  select(doy, perc)

### Output for HF ----
schematic_v2_HF <-
  ggplot() +
  # Overall theme:
  coord_cartesian(xlim = plot_xlim, ylim = c(0,1)) +
  theme_bw() +
  # Axes tick marks
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = theme.size),
    axis.text = element_text(size = theme.size),
    axis.text.x.top = element_text(color = hot_color),
    axis.ticks.x.top = element_line(color = hot_color),
    axis.ticks.x.bottom = element_line(
      color = c(rep(cold_color, 4), rep("black", 2), "white")
    ),
    axis.text.x.bottom = element_text(
      color = c(rep(cold_color, 4), rep("black", 3))
    )
  ) +
  scale_x_continuous(
    name = "Day of year",
    # Bottom cold
    breaks = c(
      HF_doy_diameter_quartile_cold %>% filter(perc %in% c(0.25, 0.5, 0.75)) %>% pull(doy),
      HF_doy_max_rate %>% filter(year == "Cold") %>% pull(doy),
      window_open,
      window_close,
      mean(c(window_close, window_open))
    ),
    labels = c(
      expression(DOY[25]),
      expression(paste("     ", DOY[50], " ", DOY[g[max]])),
      expression(DOY[75]),
      expression(paste("")),
      NA,
      NA,
      "Critical Temperature Window"
      ),
    # Top hot
    sec.axis = sec_axis(
      ~ . * 1,
      breaks = c(HF_doy_diameter_quartile_hot$doy, HF_doy_max_rate %>% filter(year == "Hot") %>% pull(doy)),
      labels = c(1, expression(DOY[25]), expression(paste("     ", DOY[50], " ", DOY[g[max]])), expression(DOY[75]), 365, expression(paste("")))
    )
  ) +
  scale_y_continuous(
    name = "Stem diameter growth",
    breaks = NULL,
    labels = NULL,
  ) +
  # Both LG5 growth curves
  geom_line(data = HF_true_values, mapping = aes(x = doy, y = perc, col = year)) +
  scale_color_manual(values = c(cold_color, hot_color)) +
  # DOY for 25%/50%/75% and g_max:
  geom_segment(
    data = HF_doy_diameter_quartile %>% filter(year == "Hot"),
    aes(x = doy, xend = doy, y = 1.25, yend = perc, col = year),
    linetype = "dashed", show.legend = FALSE, alpha = 0.5
  ) +
  geom_segment(
    data = HF_doy_diameter_quartile %>% filter(year == "Cold"),
    aes(x = doy, xend = doy, y = perc, yend = -0.25, col = year),
    linetype = "dashed", show.legend = FALSE, alpha = 0.5
  ) +
  geom_segment(
    data = HF_doy_max_rate %>% filter(year == "Hot"),
    aes(x = doy, xend = doy, y = 1.25, yend = perc, col = year),
    linetype = "dashed", show.legend = FALSE, alpha = 0.5
  ) +
  geom_segment(
    data = HF_doy_max_rate %>% filter(year == "Cold"),
    aes(x = doy, xend = doy, y = perc, yend = -0.25, col = year),
    linetype = "dashed", show.legend = FALSE, alpha = 0.5
  ) +
  # Cold/blue Primary Growing Season:
  geom_segment(
    aes(
      x = HF_doy_diameter_quartile_cold$doy[2],
      y = 0 + 0.05,
      xend = HF_doy_diameter_quartile_cold$doy[4],
      yend = 0 + 0.05
    ),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    col = cold_color
  ) +
  annotate(
    "text",
    x = HF_doy_diameter_quartile_cold$doy[2] + (HF_doy_diameter_quartile_cold$doy[4] - HF_doy_diameter_quartile_cold$doy[2]) * 0.35,
    y = 0 + 0.025,
    label = expression(L[PGS]),
    hjust = 0.5,
    size = geom.text.size,
    col = cold_color
  ) +
  # Hot/red Primary Growing Season:
  geom_segment(
    aes(
      x = HF_doy_diameter_quartile_hot$doy[2],
      y = 0.85 + 0.05,
      xend = HF_doy_diameter_quartile_hot$doy[4],
      yend = 0.85 + 0.05
    ),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both"),
    col = hot_color
  ) +
  annotate(
    "text",
    x = HF_doy_diameter_quartile_hot$doy[2] + (HF_doy_diameter_quartile_hot$doy[4] - HF_doy_diameter_quartile_hot$doy[2]) * 0.22,
    y =  0.85 + 0.025,
    label = expression(L[PGS]),
    hjust = 0.5,
    size = geom.text.size,
    col = hot_color
  ) +
  # Add critical temperature window
  # geom_vline(aes(xintercept = window_open), linetype = "dashed", alpha = 0.5) +
  # geom_vline(aes(xintercept = window_close), linetype = "dashed", alpha = 0.5) +
  # annotate(
  #   "text",
  #   x = window_open + (window_close - window_open) * 0.5,
  #   y = 0.3,
  #   label = "Critical\nTemperature\nWindow",
  #   hjust = 0.5,
  #   vjust = 1,
  #   size = geom.text.size
  # ) +
  geom_segment(
    aes(
      x = window_open,
      xend = window_close,
      y = -0.025,
      yend = -0.025
    ),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both")
  ) +
  geom_segment(
    aes(x = c(window_open, window_close), xend = c(window_open, window_close), y = c(0.25, 0.25), yend = c(-0.25, - 0.25)),
    linetype = "dashed"
  ) +
  # Horizontal significant shift arrows:
  geom_segment(
    data = HF_significant_perc,
    aes(xend = doy_hot, x = doy_cold, yend = perc, y = perc),
    size = 1,
    arrow = arrow(length = unit(0.20, "cm"), type = "closed", angle = 40),
    linejoin = "mitre"
  ) +
  # Add legend:
  labs(col = "Spring temp") +
  theme(
    legend.position = c(1-0.975, 1-0.025),
    legend.justification = c(0, 1)
  )
schematic_v2_HF


fig_width <- 8
ggsave(
  filename = "doc/manuscript/tables_figures/schematic_summary.png",
  plot = schematic_v2_HF,
  width = fig_width, height = fig_width * 9 / 16
)










# Growth_Cruves_all: Percent modeled growth and cummulative percent modeled growth ----
Wood_pheno_table_scbi <- read_csv("Data/Wood_pheno_table_SCBI_CLEAN.csv") %>%
  # Keep only RP and DP for now
  dplyr::filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

Wood_pheno_table_hf <- read_csv("Data/Wood_pheno_table_HarvardForest_CLEAN.csv") %>%
  # Keep only RP and DP for now
  dplyr::filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))


# As generated in Rscripts/SCBI_wood_phenology.R
LG5_parameter_values_scbi <- read_csv("Data/LG5_parameter_values_SCBI_CLEAN.csv")
LG5_parameter_values_hf <- read_csv("Data/LG5_parameter_values_HarvardForest_CLEAN.csv")

# Generalized 5-parameter logistic function (modified version of Sean's function)
lg5 <- function(L, K, doy_ip, r, theta, doy) {
  # For specified 5 parameters and x = doy, compute y = dbh
  dbh <- L + ((K - L) / (1 + 1/theta * exp(-(r * (doy - doy_ip) / theta)) ^ theta))
  return(dbh)
}
Wood_pheno_table_scbi <- Wood_pheno_table_scbi[,-12]
LG5_parameter_values_scbi <- LG5_parameter_values_scbi[,-11]
percent_growth_scbi <-Wood_pheno_table_scbi %>%
  mutate(
    tag = as.numeric(tag),
    tag_year = str_c(tag,year)
  ) %>%
  select(tag, year, tag_year, wood_type) %>%
  distinct() %>%
  left_join(LG5_parameter_values_scbi, by = c("tag", "year")) %>%
  group_by(tag, year) %>%
  mutate(doy = list(seq(from = 1, to = 365, by = 1))) %>%
  unnest_longer(doy) %>%
  #mutate(
  #  tag_year = tag_year.x
  #) %>%
  group_by(tag_year) %>%
  mutate(
    dbh = lg5(L, K, doy_ip, r, theta, doy),
    dbh_growth = dbh - lag(dbh),
    dbh_total_growth = K - L,
    dbh_growth_percent = dbh_growth/dbh_total_growth
  ) %>%
  dplyr::filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )

Wood_pheno_table_hf <- Wood_pheno_table_hf[,-14]
LG5_parameter_values_hf <- LG5_parameter_values_hf[,-14]
percent_growth_hf <- Wood_pheno_table_hf %>%
  #separate(tag, into = c("tag", "stem"), sep = "_") %>%
  mutate(
    site_tag = tag,
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
  dplyr::filter(!is.na(dbh_growth)) %>%
  mutate(
    dbh_growth_percent_cummulative = cumsum(dbh_growth_percent)
  )



png(filename = "doc/manuscript/tables_figures/growth_curves_all.png", width=10, height=5,
    pointsize=12, bg="transparent", units="in", res=600,
    restoreConsole=FALSE)

grid.arrange(

  ggplot(percent_growth_scbi, aes(x = doy, y = dbh_growth_percent, group = tag_year)) +
    coord_cartesian(ylim = c(0, 0.05)) +
    scale_y_continuous(labels = percent) +
    labs( x="",y = "Percent of total growth",
          title = "SCBI") +
    theme_bw() +
    theme(legend.position = c(.75,.75))+
    geom_line(alpha = 0.2, aes(col = wood_type)) +
    scale_colour_viridis_d("", end = 2/3),

  ggplot(percent_growth_hf, aes(x = doy, y = dbh_growth_percent, group = tag_year)) +
    coord_cartesian(ylim = c(0, 0.05)) +
    scale_y_continuous(labels = percent) +
    labs(x="", y = "",
         title = "Harvard Forest") +
    theme_bw() +
    theme(legend.position = "none")+
    geom_line(alpha = 0.2, aes(col = wood_type)) +
    scale_colour_viridis_d("Wood Type", end = 2/3),

  ggplot(percent_growth_scbi, aes(x = doy, y = dbh_growth_percent_cummulative, group = tag_year, col = wood_type)) +
    geom_line(alpha = 0.2) +
    scale_y_continuous(labels = percent) +
    theme_bw()+
    theme(legend.position = "none")+
    labs(x = "DOY", y = "Cummulative percent of total growth")+
    scale_colour_viridis_d("Wood Type", end = 2/3),


  ggplot(percent_growth_hf, aes(x = doy, y = dbh_growth_percent_cummulative, group = tag_year, col = wood_type)) +
    geom_line(alpha = 0.2) +
    scale_y_continuous(labels = percent) +
    theme_bw()+
    theme(legend.position = "none")+
    labs(x = "DOY", y = "")+
    scale_colour_viridis_d("Wood Type", end = 2/3),

  as.table = TRUE, nrow=2, ncol=2) ###as.table specifies order if multiple rows

dev.off()
# DOY timing figure (Replace with figure created in DOYtiming_all_years.R)----
Wood_pheno_table_scbi <- read_csv("Data/Wood_pheno_table_SCBI_CLEAN.csv") %>%
  # Keep only RP and DP for now
  dplyr::filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

Wood_pheno_table_hf <- read_csv("Data/Wood_pheno_table_HarvardForest_CLEAN.csv") %>%
  # Keep only RP and DP for now
  dplyr::filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

warmestRP_scbi <- subset(Wood_pheno_table_scbi, year == 2013 & wood_type == "ring-porous")
warmestDP_scbi <- subset(Wood_pheno_table_scbi, year == 2012 & wood_type == "diffuse-porous")
warmest_scbi <- rbind(warmestDP_scbi, warmestRP_scbi)
coldestRP_scbi <- subset(Wood_pheno_table_scbi, year == 2013 & wood_type == "ring-porous")
coldestDP_scbi <- subset(Wood_pheno_table_scbi, year == 2013 & wood_type == "diffuse-porous")
coldest_scbi <- rbind(coldestDP_scbi, coldestRP_scbi)
aggregates_scbi <- aggregate(Wood_pheno_table_scbi$DOY, by = list(Wood_pheno_table_scbi$wood_type, Wood_pheno_table_scbi$perc), FUN = mean)
#aggregates_scbi_rp <- subset(Wood_pheno_table_scbi, year == 2015 & wood_type == "ring-porous")
#aggregates_scbi_dp <- subset(Wood_pheno_table_scbi, year == 2015 & wood_type == "diffuse-porous")
#aggregates_scbi_pre <- rbind(aggregates_scbi_dp, aggregates_scbi_rp)
#aggregates_scbi <-aggregate(aggregates_scbi_pre$DOY, by = list(aggregates_scbi_pre$wood_type, aggregates_scbi_pre$perc), FUN = mean)
aggregates_scbi$temp_type <- "Average"
aggregates_warm_scbi <- aggregate(warmest_scbi$DOY, by = list(warmest_scbi$wood_type, warmest_scbi$perc), FUN = mean)
names(aggregates_warm_scbi) <- c("Group.1", "Group.2", "x")
aggregates_warm_scbi$temp_type <- "Warmest Year"
aggregates_cold_scbi <- aggregate(coldest_scbi$DOY, by = list(coldest_scbi$wood_type, coldest_scbi$perc), FUN = mean)
names(aggregates_cold_scbi) <- c("Group.1", "Group.2", "x")
aggregates_cold_scbi$temp_type <- "Coldest Year"

aggregates_scbi <- rbind(aggregates_scbi, aggregates_cold_scbi, aggregates_warm_scbi)
aggregates_scbi$Group.2 <- ifelse(aggregates_scbi$Group.2 == .25, 25, ifelse(aggregates_scbi$Group.2 == .50, 50, ifelse(aggregates_scbi$Group.2 == .75, 75, 0)))
aggregates_scbi <- aggregates_scbi %>% mutate(temp_type = factor(temp_type, levels = c("Warmest Year", "Average", "Coldest Year")))

#aggregates <- left_join(aggregates, aggregates_cold, by = c("Group.1", "Group.2"))
#aggregates <- left_join(aggregates, aggregates_warm, by = c("Group.1", "Group.2"))
#names(aggregates) <- c("Group.1", "Group.2","x","Cold", "warm")
#names(aggregates) <- c("Wood type", "Growth vari", "DOY")
doytiming_scbi <- ggplot(aggregates_scbi, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, temp_type), color = temp_type, linetype = Group.1))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "SCBI Intraannual Growth Timing", color = "Temp", linetype = "Wood Type")+
  scale_colour_manual(values = c("red", "purple", "blue"))

#geom_line(aes(x = cold), linetype = "dashed", size =1, show.legend = TRUE)+
#geom_line(aes(x = warm), linetype = "dotted", size = 1, show.legend = TRUE)+


#fig_width <- 7
#ggsave(filename = "doc/manuscript/tables_figures/DOYtiming.png",
#       plot = doytiming,
#       width = fig_width, height = fig_width / 2)

warmestRP_hf <- subset(Wood_pheno_table_hf, year == 2001 & wood_type == "ring-porous")#2001
warmestDP_hf <- subset(Wood_pheno_table_hf, year == 1998 & wood_type == "diffuse-porous")#1998
warmest_hf <- rbind(warmestRP_hf, warmestDP_hf)
coldestRP_hf <- subset(Wood_pheno_table_hf, year == 2000 & wood_type == "ring-porous")#2000
coldestDP_hf <- subset(Wood_pheno_table_hf, year == 2000 & wood_type == "diffuse-porous")#2000
coldest_hf <- rbind(coldestDP_hf, coldestRP_hf)
aggregates_hf <- aggregate(Wood_pheno_table_hf$DOY, by = list(Wood_pheno_table_hf$wood_type, Wood_pheno_table_hf$perc), FUN = mean)
aggregates_hf$temp_type <- "Average"
aggregates_warm_hf <- aggregate(warmest_hf$DOY, by = list(warmest_hf$wood_type, warmest_hf$perc), FUN = mean)
names(aggregates_warm_hf) <- c("Group.1", "Group.2", "x")
aggregates_warm_hf$temp_type <- "Warmest Year"
aggregates_cold_hf <- aggregate(coldest_hf$DOY, by = list(coldest_hf$wood_type, coldest_hf$perc), FUN = mean)
names(aggregates_cold_hf) <- c("Group.1", "Group.2", "x")
aggregates_cold_hf$temp_type <- "Coldest Year"

aggregates_hf <- rbind(aggregates_hf, aggregates_cold_hf, aggregates_warm_hf)
aggregates_hf <- aggregates_hf %>% mutate(temp_type = factor(temp_type, levels = c("Warmest Year", "Average", "Coldest Year")))

#aggregates$Group.2 <- ifelse(aggregates$Group.2 == .25, 25, ifelse(aggregates$Group.2 == .50, 50, ifelse(aggregates$Group.2 == .75, 75, 0)))

#aggregates <- left_join(aggregates, aggregates_cold, by = c("Group.1", "Group.2"))
#aggregates <- left_join(aggregates, aggregates_warm, by = c("Group.1", "Group.2"))
#names(aggregates) <- c("Group.1", "Group.2","x","Cold", "warm")
#names(aggregates) <- c("Wood type", "Growth vari", "DOY")
doytiming_hf <- ggplot(aggregates_hf, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, temp_type), color = temp_type, linetype = Group.1))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "Harvard Forest Intraannual Growth Timing", color = "Wood Type")+
  scale_colour_manual(values = c("red", "purple", "blue"))

png(filename = "doc/manuscript/tables_figures/DOYtiming.png", width=10, height=10,
    pointsize=12, bg="transparent", units="in", res=600,
    restoreConsole=FALSE)

grid.arrange(
  ggplot(aggregates_scbi, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, temp_type), color = temp_type, linetype = Group.1))+
    geom_point(size = 3)+
    geom_line(size = 1)+
    theme_bw()+
    theme(legend.position = "top")+
    labs(x = "", y = "Percent of Total Annual Growth", title = "(a) SCBI", color = "Temp", linetype = "Wood Type")+
    scale_colour_manual(values = c("red", "purple", "blue")),

  ggplot(aggregates_hf, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, temp_type), color = temp_type, linetype = Group.1))+
    geom_point(size = 3)+
    geom_line(size = 1)+
    theme_bw()+
    theme(legend.position = "none")+
    labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "(b) Harvard Forest", color = "Wood Type")+
    scale_colour_manual(values = c("red", "purple", "blue")),

  as.table = TRUE, nrow=2, ncol=1) ###as.table specifies order if multiple rows

dev.off()

#####Climwin figure ------
library(climwin)
#Edit the plotbetas function in climwin
#In the function, change scale_fill_gradientn(colours = c("red", "yellow", "blue"), name = "") to scale_fill_gradient2(high = "blue", mid = "yellow", low = "red")
#Change Circle coordinates from best window to median of windows which make up 95% cw <median(dataset[dataset$cw == 1,]$WindowOpen)>

trace("plotbetas", edit = TRUE)

#ggplot(MassOutput, aes(x = WindowClose, y = WindowOpen, z = ModelBeta)) +
#  geom_tile(aes(fill = ModelBeta)) +
#  scale_fill_gradient2(high = "blue", mid = "yellow", low = "red") +
#  #theme_climwin() +
#  theme(legend.position = c(0.75,0.3)) +
#  ggtitle("Beta linear") +
#  ylab("Window open") +
#  xlab("Window close")
cw = 0.95
SCBI_dp <- read_csv("results/Climwin_results/Weekly/SCBI/MassOutput_0.25diffuse-porous.csv")
SCBI_dp    <- SCBI_dp[order(-SCBI_dp$ModWeight), ]
SCBI_dp$cw <- as.numeric(cumsum(SCBI_dp$ModWeight) <= cw)
SCBI_dp$ID <- "SCBI Diffuse-porous"

SCBI_rp <- read_csv("results/Climwin_results/Weekly/SCBI/MassOutput_0.25ring-porous.csv")
SCBI_rp    <- SCBI_rp[order(-SCBI_rp$ModWeight), ]
SCBI_rp$cw <- as.numeric(cumsum(SCBI_rp$ModWeight) <= cw)
SCBI_rp$ID <- "SCBI Ring-porous"

HF_dp <- read_csv("results/Climwin_results/Weekly/Harvard Forest/MassOutput_0.25_diffuse-porous.csv")
HF_dp    <- HF_dp[order(-HF_dp$ModWeight), ]
HF_dp$cw <- as.numeric(cumsum(HF_dp$ModWeight) <= cw)
HF_dp$ID <- "Harvard Forest Diffuse-porous"

HF_rp <- read_csv("results/Climwin_results/Weekly/Harvard Forest/MassOutput_0.25_ring-porous.csv")
HF_rp    <- HF_rp[order(-HF_rp$ModWeight), ]
HF_rp$cw <- as.numeric(cumsum(HF_rp$ModWeight) <= cw)
HF_rp$ID <- "Harvard Forest Ring-porous"

png(filename = "doc/manuscript/tables_figures/climwin_figure.png", width=10, height=15,
    pointsize=12, bg="transparent", units="in", res=600,
    restoreConsole=FALSE)

grid.arrange(
  plotbetas(SCBI_rp, arrow = TRUE),
  plotbetas(SCBI_dp, arrow = TRUE),
  plotbetas(HF_rp, arrow = TRUE),
  plotbetas(HF_dp, arrow = TRUE),


  as.table = TRUE, nrow=2, ncol=2) ###as.table specifies order if multiple rows

dev.off()
