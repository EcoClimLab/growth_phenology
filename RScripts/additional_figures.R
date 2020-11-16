library(tidyverse)
library(scales)
library(ggrepel)
library(tidybayes)

# Figure numbers from: https://github.com/SCBI-ForestGEO/growth_phenology/issues/13

# Figure 1: Logistic growth curve and parameter illustration -------------------
# Set parameters from McMahon & Parker (2014)
# 1. L: lower asymptotes
# 2. K: upper asymptotes
# 3. doy.ip: inflection point
# 4. r: value that determines slope of curve at inflection point ((K-L)*r/theta )/(1 + 1/theta)^2
# 5. theta: theta = 1 indicates pre/post inflection post symmetry
L <- 13
K <- 14
doy.ip <- 182
r <- 0.025
theta <- 1.5
params <- c(L, K, doy.ip, r, theta)
total_growth <- K - L

start_doy <- 1
end_doy <- 365

window_open <- 60
window_close <- 105


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

# 1. True growth (logistic curve):
true_values <- tibble(
  doy = seq(from = start_doy, to = end_doy, by = 1),
  diameter = lg5.pred(params, doy)
) %>%
  mutate(diff = diameter - lag(diameter))


# 2. Max growth rate/inflection point (tangent line):
# Incorrect intercept since r is not true slope at inflection point:
# intercept <- lg5.pred(params, doy.ip) - r *doy.ip

# Version 1 of correct slope and intercept
r_true <- ((K-L)*r/theta )/(1 + 1/theta)^2
intercept_true <- lg5.pred(params, doy.ip) - r_true *doy.ip

# Version 2 of correct doy/ip, slope, and intercept
doy.ip_true <- -log(theta)/r + doy.ip
r_true_true <- r*(K-L)/4
intercept_true_true <- lg5.pred(params, doy.ip_true) - r_true_true *doy.ip_true

ggplot(true_values, aes(x = doy, y = diff)) +
  geom_line() +
  geom_vline(xintercept = doy.ip_true) +
  geom_hline(yintercept = r_true_true)


# 3. Observed values (noise/error added) (points)
set.seed(76)
observed_values <- tibble(
  doy = seq(from = start_doy + 14, to = end_doy - 14, length = 24),
  diameter = lg5.pred(params, doy)
) %>%
  mutate(diameter = diameter + rnorm(n(), sd = 0.025))



# 4. Compute 25%/50%/75% growth values and DOY
doy_diameter_quartile <- bind_rows(
  tibble(doy = 1, diameter = L),
  true_values %>% filter(diameter >= (L + total_growth * .25)) %>% slice(1),
  true_values %>% filter(diameter >= (L + total_growth * .5)) %>% slice(1),
  true_values %>% filter(diameter >= (L + total_growth * .75)) %>% slice(1),
  tibble(doy = 365, diameter = K)
) %>%
  mutate(
    growth = c(0, 0.25, 0.5, 0.75, 1),
    label = c("0%", "25%", "50%", "75%", "100%"),
    label = factor(label, levels = c("0%", "25%", "50%", "75%", "100%"))
  )

# Function to create 25%/50%/75% growth polygons
create_growth_polygon <- function(doy_diameter_quartile, percent){
  index <- case_when(
    percent == 0.25 ~ 1,
    percent == 0.5 ~ 2,
    percent == 0.75 ~ 3
  )
  growth_doy_domain <- seq(from = doy_diameter_quartile$doy[index], to = doy_diameter_quartile$doy[4])
  n_days_domain <- length(growth_doy_domain)

  growth_polygon <- tibble(
    doy = c(growth_doy_domain[1], growth_doy_domain, growth_doy_domain[n_days_domain], growth_doy_domain[1]),
    diameter = c(L, lg5.pred(params, growth_doy_domain), L, L)
  )

  return(growth_polygon)
}


# Output figure
geom.text.size <- 4
theme.size <- (14/5) * geom.text.size


schematic <- ggplot() +
  # Overall theme:
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = theme.size),
    axis.title = element_text(size = theme.size)
  ) +
  labs(
    x = "Day of year (1 to 365)",
    y = "Diameter at breast height"
  ) +
  coord_cartesian(xlim = c(30, 280))+
  # Mark DOY's on x-axis:
  geom_vline(data = doy_diameter_quartile, aes(xintercept = doy), linetype = "dashed", show.legend = FALSE, col = "grey") +
  scale_x_continuous(
    breaks = c(doy_diameter_quartile$doy, doy.ip, window_open, window_close),
    labels = c(1, expression(DOY[25]), expression(DOY[50]), expression(DOY[75]), 365, expression(DOY[ip]), expression(w[open]), expression(w[close]))
  ) +
  # Mark growth percentages on y-axis:
  geom_hline(data = doy_diameter_quartile, aes(yintercept = diameter), linetype = "dashed", show.legend = FALSE, col = "grey") +
  scale_y_continuous(
    breaks = c(L, K),
    labels = c("L", "K"),
    sec.axis = sec_axis(
      ~ . * 1,
      breaks = doy_diameter_quartile$diameter ,
      labels = doy_diameter_quartile$label,
      name = expression(paste("% of (fitted) annual growth ", Delta[DBH],  " = K - L"))
    )
  ) +
  # Inflection point & max growth rate
  annotate("point", x = doy.ip, y = lg5.pred(params, doy.ip), shape = 18, size = 4) +
  geom_vline(xintercept = doy.ip, linetype = "dotted") +
  geom_abline(intercept = intercept_true, slope = r_true, linetype = "dotted") +
  annotate(
    "text",
    x = doy.ip,
    y = lg5.pred(params, doy.ip) + 0.025,
    label = expression(paste(g[max], " = slope of tangent line  ")),
    hjust = 1,
    size = geom.text.size
  ) +
  # Growth window:
  geom_segment(
    aes(
      x = doy_diameter_quartile$doy[2],
      y = L + 0.05,
      xend = doy_diameter_quartile$doy[4],
      yend = L + 0.05
    ),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both")
  ) +
  annotate(
    "text",
    x = doy_diameter_quartile$doy[2] + (doy_diameter_quartile$doy[4] - doy_diameter_quartile$doy[2]) * 0.5,
    y = L + 0.025,
    label = expression(L[PGS]),
    hjust = 0.5,
    size = geom.text.size
  ) +
  # Critical temperature window:
  geom_rect(aes(xmin = window_open, xmax = window_close, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  geom_segment(
    aes(
      x = window_open,
      y = L + (K-L) * 0.45 + 0.025,
      xend = window_close,
      yend = L + (K-L) * 0.45 + 0.025
    ),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both")
  ) +
  annotate(
    "text",
    x = window_open + (window_close - window_open) * 0.5,
    y = L + (K-L) * 0.45,
    label = "Critical\nTemperature\nWindow",
    hjust = 0.5,
    vjust = 1,
    size = geom.text.size
  ) +
  # True growth curve
  geom_line(data = true_values, mapping = aes(x = doy, y = diameter)) +
  # Observed values
  geom_point(data = observed_values, mapping = aes(x = doy, y = diameter), shape = 21, colour = "black", fill = "white", size = 3, stroke = 1)

schematic
fig_width <- 9
ggsave("doc/manuscript/tables_figures/schematic.png", plot = schematic, width = fig_width, height = fig_width * 9 / 16)





#----





# Growth_Cruves_all: Percent modeled growth and cummulative percent modeled growth ----
Wood_pheno_table_scbi <- read_csv("Data/Wood_pheno_table_V13CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

Wood_pheno_table_hf <- read_csv("Data/Wood_pheno_table_HarvardForest_V9CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))


# As generated in Rscripts/SCBI_wood_phenology.R
LG5_parameter_values_scbi <- read_csv("Data/LG5_parameter_values_V13CLEAN.csv")
LG5_parameter_values_hf <- read_csv("Data/LG5_parameter_values_HarvardForest_V9CLEAN.csv")

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
  filter(!is.na(dbh_growth)) %>%
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
  filter(!is.na(dbh_growth)) %>%
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
# ----
#DOY timing figure----
Wood_pheno_table_scbi <- read_csv("Data/Wood_pheno_table_V13CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

Wood_pheno_table_hf <- read_csv("Data/Wood_pheno_table_HarvardForest_V9CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

warmestRP_scbi <- subset(Wood_pheno_table_scbi, year == 2012 & wood_type == "ring-porous")
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
#names(aggregates) <- c("Group.1", "Group.2","x","cold", "warm")
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
coldestRP_hf <- subset(Wood_pheno_table_hf, year == 2000 & wood_type == "ring-porous")#2003
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
#names(aggregates) <- c("Group.1", "Group.2","x","cold", "warm")
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








