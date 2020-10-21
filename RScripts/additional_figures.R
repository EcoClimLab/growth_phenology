library(tidyverse)
library(scales)
library(tidybayes)

# Figure numbers from: https://github.com/SCBI-ForestGEO/growth_phenology/issues/13

# Figure 1: Logistic growth curve and parameter illustration -------------------
# Set parameters from McMahon & Parker (2014)
# 1. L: lower asymptotes
# 2. K: upper asymptotes
# 3. doy.ip: inflection point
# 4. r: slope of curve at inflection point
# 5. theta: theta = 1 indicates pre/post inflection post symmetry
L <- 13.2
K <- 13.8
doy.ip <- 175
r <- 0.05
theta <- 1
params <- c(L, K, doy.ip, r, theta)

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

# Compute true values to plot true growth curve
true_values <- tibble(
  doy = seq(from = 100, to = 300),
  diameter = lg5.pred(params, doy)
)

# Max Growth rate
intercept <- lg5.pred(params, doy.ip) - r *doy.ip

r_true <- ((K-L)*r/theta )/(1 + 1/theta)^2
intercept_true <- lg5.pred(params, doy.ip) - r_true *doy.ip

offset <- 40
max_growth_rate <- tibble(
  doy = c(doy.ip - offset, doy.ip + offset),
) %>%
  mutate(
    diameter = doy * r_true + intercept_true
  )





# Compute observed values (noise/error added) to plot with points
set.seed(76)
observed_values <- tibble(
  doy = seq(from = 105, to = 295, length = 13),
  diameter = lg5.pred(params, doy)
) %>%
  mutate(diameter = diameter + rnorm(n(), sd = 0.025))

# Compute DOY of diameter quartiles (based on true values?)
doy_diameter_quartile <- bind_rows(
  true_values %>% filter(diameter >= quantile(diameter, probs = 0.25)) %>% slice(1),
  true_values %>% filter(diameter >= quantile(diameter, probs = 0.50)) %>% slice(1),
  true_values %>% filter(diameter >= quantile(diameter, probs = 0.75)) %>% slice(1),
  true_values %>% filter(diameter >= quantile(diameter, probs = 1)) %>% slice(1)
) %>%
  mutate(
    growth = c(0.25, 0.5, 0.75, 1),
    label = c("25%", "50%", "75%", "100% of growth")
  )

# Function to create polygons to ggplot
create_growth_polygon <- function(percent){
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
schematic <- ggplot() +
  # 25%, 50%, 75%, 100% polygons & labels
  geom_polygon(data = create_growth_polygon(percent = 0.25), mapping = aes(x = doy, y = diameter), fill = grey(level = 0.75)) +
  geom_polygon(data = create_growth_polygon(percent = 0.50), mapping = aes(x = doy, y = diameter), fill = grey(level = 0.625)) +
  geom_polygon(data = create_growth_polygon(percent = 0.75), mapping = aes(x = doy, y = diameter), fill = grey(level = 0.5)) +
  geom_text(data = doy_diameter_quartile, aes(x = doy - 3, label = label), y = 13.21, angle = 90, hjust = 0, size = 7.5) +
  # L & K asymptotes
  geom_hline(yintercept = L, linetype = "dashed", col = "grey") +
  annotate("text", x = true_values$doy[1], y = L, label = "L", size = 7.5) +
  geom_hline(yintercept = K, linetype = "dashed", col = "grey") +
  annotate("text", x = true_values$doy[1], y = K, label = "K", size = 7.5) +
  # True growth curve
  geom_line(data = true_values, mapping = aes(x = doy, y = diameter)) +
  # Observed values
  geom_point(data = observed_values, mapping = aes(x = doy, y = diameter), shape = 21, colour = "black", fill = "white", size = 3, stroke = 1) +
  # Inflection point & max growth rate
  annotate("point", x = doy.ip, y = lg5.pred(params, doy.ip), size = 4) +
  annotate("text", x = doy.ip + 3, y = lg5.pred(params, doy.ip), label = "inflection point:\nmax growth rate\n(dotted slope)", hjust = 0, size = 4) +
  geom_line(data = max_growth_rate, aes(x = doy, y = diameter), linetype = "dotted") +
  # geom_abline(intercept = intercept, slope = r) +
  # Total growth arrows:
  geom_segment(aes(x = doy_diameter_quartile$doy[4] + 10, y = K, xend = doy_diameter_quartile$doy[4] + 10, yend = L), arrow = arrow(length = unit(0.5, "cm"), ends = "both")) +
  annotate("text", x = doy_diameter_quartile$doy[4] + 7, y = L + (K - L) / 2, label = "Total annual growth", angle = 90, vjust = 0.5, size = 7.5) +
  # Growth window arrows:
  geom_segment(aes(
    x = doy_diameter_quartile$doy[1],
    y = L - 0.05,
    xend = doy_diameter_quartile$doy[3],
    yend = L - 0.05
  ),
  arrow = arrow(length = unit(0.5, "cm"), ends = "both")
  ) +
  annotate("text",
    x = doy_diameter_quartile$doy[1] + (doy_diameter_quartile$doy[3] - doy_diameter_quartile$doy[1]) / 2,
    y = L - 0.025,
    label = "75% - 25% growth window", hjust = 0.5, size = 7.5
  ) +
  # Etc
  labs(x = "Day of year", y = "Diameter (cm)") +
  theme_bw()
schematic

fig_width <- 11
ggsave("doc/manuscript/tables_figures/schematic.png", plot = schematic, width = fig_width, height = fig_width / 1.25)






# Fig2 & Fig3: Percent modeled growth and cummulative percent modeled growth ---
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V7.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))


# As generated in Rscripts/SCBI_wood_phenology.R
LG5_parameter_values <- read_csv("Data/LG5_parameter_values.csv")

# Generalized 5-parameter logistic function (modified version of Sean's function)
lg5 <- function(L, K, doy_ip, r, theta, doy) {
  # For specified 5 parameters and x = doy, compute y = dbh
  dbh <- L + ((K - L) / (1 + 1/theta * exp(-(r * (doy - doy_ip) / theta)) ^ theta))
  return(dbh)
}

percent_growth <- Wood_pheno_table %>%
  separate(tag, into = c("tag", "stem"), sep = "_") %>%
  mutate(
    tag = as.numeric(tag),
    tag_year = str_c(tag, year)
  ) %>%
  select(tag, year, tag_year, wood_type) %>%
  distinct() %>%
  left_join(LG5_parameter_values, by = c("tag", "year")) %>%
  group_by(tag, year) %>%
  mutate(doy = list(seq(from = 1, to = 365, by = 1))) %>%
  unnest_longer(doy) %>%
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
ggsave(filename = "doc/manuscript/tables_figures/rel_growth.png",
       plot = rel_growth,
       width = fig_width, height = fig_width / 2)



fig3 <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent_cummulative, group = tag_year, col = wood_type)) +
  geom_line(alpha = 0.2) +
  scale_y_continuous(labels = percent) +
  labs(x = "DOY", y = "Cummulative percent of total growth", title = "Cummulative percent of total (modeled) growth in diameter")
fig3

fig_width <- 6
ggsave(filename = "doc/manuscript/tables_figures/fig3.png", plot = fig3, width = fig_width, height = fig_width / 1.52)



# fig3b <- ggplot(percent_growth, aes(x = doy, y = dbh_growth_percent_cummulative, group = wood_type)) +
#   stat_lineribbon(.width = c(.95, 0.9),  color = "#08519C") +
#   facet_wrap(~wood_type, ncol = 1) +
#   scale_y_continuous(labels = percent) +
#   scale_fill_brewer() +
#   labs(x = "DOY", y = "Cummulative percent of total growth", title = "Cummulative percent of total (modeled) growth in diameter", subtitle = "Each curve represents one tag-year")
# fig3b

#Figure D'Orangeville figure 4
warmest <- subset(Wood_pheno_table, year == 2012)
coldestRP <- subset(Wood_pheno_table, year == 2013 & wood_type == "ring-porous")
coldestDP <- subset(Wood_pheno_table, year == 2018 & wood_type == "diffuse-porous")
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
aggregates$Group.2 <- ifelse(aggregates$Group.2 == .25, 25, ifelse(aggregates$Group.2 == .50, 50, ifelse(aggregates$Group.2 == .75, 75, 0)))
#aggregates <- left_join(aggregates, aggregates_cold, by = c("Group.1", "Group.2"))
#aggregates <- left_join(aggregates, aggregates_warm, by = c("Group.1", "Group.2"))
#names(aggregates) <- c("Group.1", "Group.2","x","cold", "warm")
#names(aggregates) <- c("Wood type", "Growth vari", "DOY")
doytiming <- ggplot(aggregates, aes(x=x, y = as.character(Group.2), group = interaction(Group.1, temp_type), color = Group.1, linetype = temp_type))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  labs(x = "Day of Year", y = "Percent of Total Annual Growth", title = "SCBI Intraannual Growth Timing", color = "Wood Type")+
  scale_colour_viridis_d(end = 2/3)

  #geom_line(aes(x = cold), linetype = "dashed", size =1, show.legend = TRUE)+
  #geom_line(aes(x = warm), linetype = "dotted", size = 1, show.legend = TRUE)+


fig_width <- 7
ggsave(filename = "doc/manuscript/tables_figures/DOYtiming.png",
       plot = doytiming,
       width = fig_width, height = fig_width / 2)
