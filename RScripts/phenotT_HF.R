#Create fig 6
# Load packages and data ---------------------------------------
library(tidyverse)
library(lubridate)
library(tidybayes)
library(patchwork)
library(knitr)
library(scales)
options(mc.cores = parallel::detectCores())
library(rstanarm)
library(broom.mixed)
#Harvard Forest----
# Get growth data ----------------------------------
Wood_pheno_table_hf <- read_csv("Data/Wood_pheno_table_HarvardForest_V9CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  #filter(tot >= 1) %>%
  #filter(tot <= 12.06)%>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))
#Wood_pheno_table$tag <- substr(Wood_pheno_table$tag,1, nchar(as.character(Wood_pheno_table$tag))-4)

twofive_hf <- subset(Wood_pheno_table_hf, perc == .25)
fifty_hf <- subset(Wood_pheno_table_hf, perc == .5)
sevenfive_hf <- subset(Wood_pheno_table_hf, perc == .75)
#25-50
twofifty_hf <- cbind(twofive_hf,fifty_hf$DOY)
twofifty_hf$twentyfive_to_fifty <- twofifty_hf$`fifty_hf$DOY`-twofifty_hf$DOY
twofifty_hf <- twofifty_hf[,c(3,6,17)]
#50-75
fiftyseventy_hf <- cbind(fifty_hf, sevenfive_hf$DOY)
fiftyseventy_hf$fifty_to_seventy <- fiftyseventy_hf$`sevenfive_hf$DOY`-fiftyseventy_hf$DOY
fiftyseventy_hf <- fiftyseventy_hf[,c(3,6,17)]
#25-75
twosevenfive_hf <- cbind(twofive_hf, sevenfive_hf$DOY)
twosevenfive_hf$seasonlength <- twosevenfive_hf$`sevenfive_hf$DOY`-twosevenfive_hf$DOY
twosevenfive_hf <- twosevenfive_hf[,c(3,6,17)]


# Create temperature variables ----------------------------------
# 0. Get all weather data
weatherdata_hf <-
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
climwindows_hf <-
  read.csv("results/Climwin_results/Weekly/Harvard Forest/weekly_climwin_results_all_HF_975.csv") %>%
  filter(wood_type != "other") %>%
  mutate(
    median_windowopendate = as.Date(median_windowopendate, format = "%Y-%m-%d"),
    median_windowclosedate = as.Date(median_windowclosedate, format = "%Y-%m-%d"),
    opendoy = yday(median_windowopendate),
    closedoy = yday(median_windowclosedate)
  )


# 1. Get mean march daily maximum temperatures
marchmeans_hf <- weatherdata_hf %>%
  filter(month == 3) %>%
  group_by(year) %>%
  summarize(marchmean = mean(airtmax))

# 2.a) EDA of climwin windows
# RP climwin window is around 3/15 to 4/23
#weatherdata %>%
#  filter(flagrp == "RP") %>%
#  mutate(DOY = yday(DATE)) %>%
#  arrange(DOY) %>%
#  slice(c(1, n()))

# DP climwin window is around 3/27 to 6/2
#weatherdata %>%
#  filter(flagdp == "DP") %>%
#  mutate(DOY = yday(DATE)) %>%
#  arrange(DOY) %>%
#  slice(c(1, n()))

climwin_windows_hf <-
  tibble(
    wood_type = c("diffuse-porous", "ring-porous"),
    window = c("climwin window: 3/19 - 5/7", "climwin window: 3/19 - 4/16")
  )


# 2.b) Get mean climwin daily maximum temperatures
# RP separately
#climwinmeans_rp <- weatherdata %>%
#  filter(flagrp == "RP") %>%
#  group_by(year) %>%
#  summarize(climwinmean = mean(TMAX)) %>%
#  mutate(wood_type = "ring-porous")
climwinmeans_rp_hf <- weatherdata_hf %>%
  filter(DOY %in% c(climwindows_hf[4,11]:climwindows_hf[4,12])) %>%
  group_by(year) %>%
  summarize(climwinmean = mean(airtmax)) %>%
  mutate(wood_type = "ring-porous")
# DP separately
#climwinmeans_dp <- weatherdata %>%
#  filter(flagdp == "DP") %>%
#  group_by(year) %>%
#  summarize(climwinmean = mean(TMAX)) %>%
#  mutate(wood_type = "diffuse-porous")
climwinmeans_dp_hf <- weatherdata_hf %>%
  filter(DOY %in% c(climwindows_hf[1,11]:climwindows_hf[1,12])) %>% #68:135
  group_by(year) %>%
  summarize(climwinmean = mean(airtmax)) %>%
  mutate(wood_type = "diffuse-porous")

# Combine
climwinmeans_hf <- bind_rows(climwinmeans_rp_hf, climwinmeans_dp_hf)


# 3. Add to growth data
Wood_pheno_table_hf <- Wood_pheno_table_hf %>%
  left_join(marchmeans_hf, by = "year") %>%
  left_join(climwinmeans_hf, by = c("year", "wood_type")) %>%
  left_join(twosevenfive_hf, by = c("tag", "year"))%>%
  left_join(fiftyseventy_hf, by = c("tag", "year")) %>%
  left_join(twofifty_hf, by = c("tag", "year")) %>%
  # Remove other variables
  #select(-c(tot, dbh, max_rate_DOY, max_rate)) %>%
  mutate(
    perc = case_when(
      perc == 0.25 ~ "DOY_25",
      perc == 0.5 ~ "DOY_50",
      perc == 0.75 ~ "DOY_75"
    )
  ) %>%
  arrange(tag, year)

# Fit multivariate model using climwinmeans ------------------------------------
# Delete all non-needed columns
Wood_pheno_table_hf_2 <- Wood_pheno_table_hf %>%
  select(perc, tag, year, wood_type, sp, climwinmean, starts_with("DOY"), site)


# Convert to wide format for use in rstanarm::stan_mvmer()
Wood_pheno_table_wide_hf <- Wood_pheno_table_hf_2 %>%
  pivot_wider(names_from = perc, values_from = DOY)

# Fit multivariate model
joint_model_climwinmeans_hf <- stan_mvmer(
  formula = list(
    DOY_25 ~ wood_type + wood_type:climwinmean + (1|site) + (1|tag),
    DOY_50 ~ wood_type + wood_type:climwinmean + (1|site) + (1|tag),
    DOY_75 ~ wood_type + wood_type:climwinmean + (1|site) + (1|tag)
  ),
  # Note we transform the data from tall/tidy format to wide format first:
  data = Wood_pheno_table_wide_hf,
  seed = 76,
  # Once we feel good about our results, increase these values. The code will
  # take longer to run however
  chains = 2,
  iter = 4000
)

# Fig6: Plot of regression of DOY over climwinmeans with credible intervals ----
# Extract predicted DOY_25, DOY_50, DOY_75
y_hat_hf <- c(
  joint_model_climwinmeans_hf %>% posterior_predict(m = 1) %>% c(),
  joint_model_climwinmeans_hf %>% posterior_predict(m = 2) %>% c(),
  joint_model_climwinmeans_hf %>% posterior_predict(m = 3) %>% c()
)
#memory.limit(size = 15000)
predictions_hf <- Wood_pheno_table_hf %>%
  add_predicted_draws(joint_model_climwinmeans_hf) %>%
  ungroup() %>%
  # Critical: sort by y outcome category (DOY_25, DOY_50, DOY_75) first
  arrange(perc, tag, year) %>%
  mutate(predictions_rstanarm = y_hat_hf)

predictions_hf %>%
  group_by(perc) %>%
  summarize(
    # Observed data values:
    mean_observed = mean(DOY),
    # Incorrect predictions generated by tidybayes::add_predicted_draws()
    # See https://github.com/mjskay/tidybayes/issues/271
    mean_predicted = mean(.prediction),
    # Correct predictions as generated by rstanarm
    mean_predicted_rstanarm = mean(predictions_rstanarm)
  )


predictions_RP_hf <- subset(predictions_hf, wood_type == "ring-porous")
predictions_DP_hf <- subset(predictions_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_hf <- subset(Wood_pheno_table_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_hf <- subset(Wood_pheno_table_hf, wood_type == "diffuse-porous")

#fig6_RP_hf <- ggplot() +
#  #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
#  stat_lineribbon(data = predictions_RP_hf, aes(x = climwinmean, y = predictions_rstanarm, group = perc, col = perc), .width = c(.99, .95)) +
#  geom_point(data = Wood_pheno_table_RP_hf, aes(x = climwinmean, y = DOY, col = perc)) +
#  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
#  scale_fill_brewer() +
#  #facet_grid(perc) +
#  coord_cartesian(xlim =c(12.3, 16), ylim = c(44, 261))+
#  theme(legend.position = "none")+
#  labs(x = "", y = "DOY", col = "Percentile", title  = "Ring-porous", subtitle = "Relationship of DOY versus climwin mean temperature")
#geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
#fig6_RP_hf

#fig6_DP_hf <- ggplot() +
#  #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
#  stat_lineribbon(data = predictions_DP_hf, aes(x = climwinmean, y = predictions_rstanarm, group = perc, col = perc), .width = c(.99, .95)) +
#  geom_point(data = Wood_pheno_table_DP_hf, aes(x = climwinmean, y = DOY, col = perc)) +
#  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
#  scale_fill_brewer() +
#  theme(legend.position = c(.95,.5))+
#  #facet_grid(perc) +
#  coord_cartesian(xlim =c(11.7, 15), ylim = c(80, 240))+
#  labs(x = "", y = "", col = "Percentile", title = "Diffuse-porous", subtitle = "Relationship of DOY versus climwin mean temperature")
##geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
#fig6_DP_hf
###TOTAL GROWTH----
woodtable_hf <- subset(Wood_pheno_table_hf, perc == "DOY_25")
total_formulaRP <- "dbh_total_growth ~ wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_total_hf <- stan_lmer(
  formula = total_formulaRP,
  data = woodtable_hf,
  seed = 349,
  iter = 4000,
  chains = 2
)

y_hot_hf <- mixedmodel_stanlmerRP_total_hf %>% posterior_predict() %>% c()

predictions_tot_hf <- woodtable_hf %>%
  add_predicted_draws(mixedmodel_stanlmerRP_total_hf) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hot_hf)

predictions_tot_RP_hf <- subset(predictions_tot_hf, wood_type == "ring-porous")
predictions_tot_DP_hf <- subset(predictions_tot_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_tot_hf <- subset(woodtable_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_tot_hf <- subset(woodtable_hf, wood_type == "diffuse-porous")
#memory.limit(size = 10000)
#fig6_RP_tot_hf <- ggplot() +
#  #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
#  stat_lineribbon(data = predictions_tot_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
#  geom_point(data = Wood_pheno_table_RP_tot_hf, aes(x = climwinmean, y = tot)) +
#  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
#  scale_fill_brewer() +
#  #facet_grid(perc) +
#  coord_cartesian(xlim =c(12.3, 16), ylim = c(-.27, 1.28))+
#  theme(legend.position = "none")+
#  labs(x = "", y = "Total (cm)", subtitle  = "Total Growth")
##geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
#fig6_RP_tot_hf

#fig6_DP_tot_hf <- ggplot() +
#  #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
#  stat_lineribbon(data = predictions_tot_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
#  geom_point(data = Wood_pheno_table_DP_tot_hf, aes(x = climwinmean, y = tot)) +
#  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
#  scale_fill_brewer() +
#  #facet_grid(perc) +
#  coord_cartesian(xlim =c(11.7, 15), ylim = c(-.27, 1))+
#  theme(legend.position = "none")+
#  labs(x = "", y = "", subtitle  = "Total Growth")
##geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
#fig6_DP_tot_hf

#Season length ----
seasonlength_formulaRP <- "seasonlength ~ wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_seasonlength_hf <- stan_lmer(
  formula = seasonlength_formulaRP,
  data = woodtable_hf,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_seasonlength_hf %>%
  tidy(conf.int = TRUE)

y_hit_hf <- mixedmodel_stanlmerRP_seasonlength_hf %>% posterior_predict() %>% c()

predictions_sl_hf <- woodtable_hf %>%
  add_predicted_draws(mixedmodel_stanlmerRP_seasonlength_hf) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hit_hf)

predictions_sl_RP_hf <- subset(predictions_sl_hf, wood_type == "ring-porous")
predictions_sl_DP_hf <- subset(predictions_sl_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_sl_hf <- subset(woodtable_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_sl_hf <- subset(woodtable_hf, wood_type == "diffuse-porous")

#fig6_RP_sl_hf <- ggplot() +
#  #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
#  stat_lineribbon(data = predictions_sl_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
#  geom_point(data = Wood_pheno_table_RP_sl_hf, aes(x = climwinmean, y = seasonlength)) +
#  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
#  scale_fill_brewer() +
#  #facet_grid(perc) +
#  coord_cartesian(xlim =c(12.3, 16), ylim = c(3, 106))+
#  theme(legend.position = "none")+
#  labs(x = "", y = "# of Days", subtitle  = "Season Length")
##geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
#fig6_RP_sl_hf

#fig6_DP_sl_hf <- ggplot() +
#  #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
#  stat_lineribbon(data = predictions_sl_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
#  geom_point(data = Wood_pheno_table_DP_sl_hf, aes(x = climwinmean, y = seasonlength)) +
#  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
#  scale_fill_brewer() +
#  #facet_grid(perc) +
#  coord_cartesian(xlim =c(11.7, 15), ylim = c(3, 106))+
#  theme(legend.position = "none")+
#  labs(x = "", y = "", subtitle  = "Season Length")
##geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
#fig6_DP_sl_hf


#MAX RATE ----
maxrate_formulaRP <- "max_rate ~ wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_maxrate_hf <- stan_lmer(
  formula = maxrate_formulaRP,
  data = woodtable_hf,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_maxrate_hf %>%
  tidy(conf.int = TRUE)

y_het_hf <- mixedmodel_stanlmerRP_maxrate_hf %>% posterior_predict() %>% c()

predictions_mr_hf <- woodtable_hf %>%
  add_predicted_draws(mixedmodel_stanlmerRP_maxrate_hf) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_het_hf)

predictions_mr_RP_hf <- subset(predictions_mr_hf, wood_type == "ring-porous")
predictions_mr_DP_hf <- subset(predictions_mr_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_mr_hf <- subset(woodtable_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_mr_hf <- subset(woodtable_hf, wood_type == "diffuse-porous")

#fig6_RP_mr_hf<- ggplot() +
#  #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
#  stat_lineribbon(data = predictions_mr_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
#  geom_point(data = Wood_pheno_table_RP_mr_hf, aes(x = climwinmean, y = max_rate)) +
#  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
#  scale_fill_brewer() +
#  #facet_grid(perc) +
#  coord_cartesian(xlim =c(12.3, 16), ylim = c(-.007, 0.02))+
#  theme(legend.position = "none")+
#  labs(x = "", y = "Growth Rate (cm/day)", subtitle  = "Maximum Growth Rate")
##geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
#fig6_RP_mr_hf

#fig6_DP_mr_hf <- ggplot() +
#  #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
#  stat_lineribbon(data = predictions_mr_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
#  geom_point(data = Wood_pheno_table_DP_mr_hf, aes(x = climwinmean, y = max_rate)) +
#  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
#  scale_fill_brewer() +
#  #facet_grid(perc) +
#  coord_cartesian(xlim =c(11.7, 15), ylim = c(-.007, 0.02))+
#  theme(legend.position = "none")+
#  labs(x = "", y = "", subtitle  = "Maximum Growth Rate")
##geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
#fig6_DP_mr_hf


#MAX RATE DOY ----
maxrateDOY_formulaRP <- "max_rate_DOY ~wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_maxrateDOY_hf <- stan_lmer(
  formula = maxrateDOY_formulaRP,
  data = woodtable_hf,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_maxrateDOY_hf %>%
  tidy(conf.int = TRUE)

y_hst_hf <- mixedmodel_stanlmerRP_maxrateDOY_hf %>% posterior_predict() %>% c()

predictions_mrdoy_hf <- woodtable_hf %>%
  add_predicted_draws(mixedmodel_stanlmerRP_maxrateDOY_hf) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hst_hf)
#memory.limit(10000)
#rm(fifty)
#rm(fifty_hf)
#rm(predictions_DP_hf)
#rm(climwinmeans)
#rm(climwinmeans_dp)
#rm(climwinmeans_dp_hf)
#rm(climwinmeans_hf)
#rm(climwinmeans_rp)
#rm(climwinmeans_rp_hf)
#rm(marchmeans)
#rm(marchmeans_hf)
#rm(predictions)
#rm(mixedmodel_stanlmerRP_total_hf)
#rm(mixedmodel_stanlmerRP_maxrate)
#rm(mixedmodel_stanlmerRP_maxrate_hf)
#rm(mixedmodel_stanlmerRP_maxrateDOY)
#rm(mixedmodel_stanlmerRP_seasonlength)
#rm(mixedmodel_stanlmerRP_seasonlength_hf)
#rm(mixedmodel_stanlmerRP_total)
#gc()
#save.image()
predictions_mrdoy_RP_hf <- subset(predictions_mrdoy_hf, wood_type == "ring-porous")
predictions_mrdoy_DP_hf <- subset(predictions_mrdoy_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_mrdoy_hf <- subset(woodtable_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_mrdoy_hf <- subset(woodtable_hf, wood_type == "diffuse-porous")

#fig6_RP_mrdoy_hf <- ggplot() +
#  #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
#  stat_lineribbon(data = predictions_mrdoy_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
#  geom_point(data = Wood_pheno_table_RP_mrdoy_hf, aes(x = climwinmean, y = max_rate_DOY)) +
#  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
#  scale_fill_brewer() +
#  #facet_grid(perc) +
#  coord_cartesian(xlim =c(12.3, 16), ylim = c(99, 235))+
#  theme(legend.position = "none")+
#  labs(x = "Temperature (C)", y = "")
##geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
#fig6_RP_mrdoy_hf

#fig6_DP_mrdoy_hf <- ggplot() +
#  #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
#  stat_lineribbon(data = predictions_mrdoy_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
#  geom_point(data = Wood_pheno_table_DP_mrdoy_hf, aes(x = climwinmean, y = max_rate_DOY)) +
#  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
#  scale_fill_brewer() +
#  #facet_grid(perc) +
#  coord_cartesian(xlim =c(11.7, 15), ylim = c(99, 235))+
#  theme(legend.position = "none")+
#  labs(x = "Temperature (C)",y= "")
##geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
#fig6_DP_mrdoy_hf

#ALL TOGETHER----
library(gridExtra)
png(filename = "doc/manuscript/tables_figures/pheno_Tsensitivity_harvardforest.png", width=15, height=25,
    pointsize=12, bg="transparent", units="in", res=600,
    restoreConsole=FALSE)

grid.arrange(

  ggplot() +
    #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_RP_hf, aes(x = climwinmean, y = predictions_rstanarm, group = perc, col = perc), .width = c(.99, .95)) +
    geom_point(data = Wood_pheno_table_RP_hf, aes(x = climwinmean, y = DOY, col = perc)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    #facet_grid(perc) +
    coord_cartesian(xlim =c(12.3, 16), ylim = c(44, 261))+
    theme(legend.position = "none")+
    labs(x = "", y = "", col = "Percentile", title  = "Harvard Forest", subtitle = "Ring-porous"),

  ggplot() +
    #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_DP_hf, aes(x = climwinmean, y = predictions_rstanarm, group = perc, col = perc), .width = c(.99, .95)) +
    geom_point(data = Wood_pheno_table_DP_hf, aes(x = climwinmean, y = DOY, col = perc)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    theme(legend.position = c(.95,.5))+
    #facet_grid(perc) +
    coord_cartesian(xlim =c(11.7, 15.5), ylim = c(80, 240))+
    labs(x = "", y = "", col = "Percentile", title = "Harvard Forest", subtitle = "Diffuse-porous"),
  ggplot() +
    #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_tot_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
    geom_point(data = Wood_pheno_table_RP_tot_hf, aes(x = climwinmean, y = tot)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    #facet_grid(perc) +
    coord_cartesian(xlim =c(12.3, 16), ylim = c(-.27, 1.28))+
    theme(legend.position = "none")+
    labs(x = "", y = ""),

  ggplot() +
    #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_tot_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
    geom_point(data = Wood_pheno_table_DP_tot_hf, aes(x = climwinmean, y = tot)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    #facet_grid(perc) +
    coord_cartesian(xlim =c(11.7, 15), ylim = c(-.27, 1))+
    theme(legend.position = "none")+
    labs(x = "", y = ""),
  ggplot() +
    #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_sl_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
    geom_point(data = Wood_pheno_table_RP_sl_hf, aes(x = climwinmean, y = seasonlength)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    #facet_grid(perc) +
    coord_cartesian(xlim =c(12.3, 16), ylim = c(3, 106))+
    theme(legend.position = "none")+
    labs(x = "", y = ""),

  ggplot() +
    #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_sl_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
    geom_point(data = Wood_pheno_table_DP_sl_hf, aes(x = climwinmean, y = seasonlength)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    #facet_grid(perc) +
    coord_cartesian(xlim =c(11.7, 15), ylim = c(3, 106))+
    theme(legend.position = "none")+
    labs(x = "", y = ""),
  ggplot() +
    #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mr_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
    geom_point(data = Wood_pheno_table_RP_mr_hf, aes(x = climwinmean, y = max_rate)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    #facet_grid(perc) +
    coord_cartesian(xlim =c(12.3, 16), ylim = c(-.007, 0.02))+
    theme(legend.position = "none")+
    labs(x = "", y = ""),

  ggplot() +
    #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mr_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
    geom_point(data = Wood_pheno_table_DP_mr_hf, aes(x = climwinmean, y = max_rate)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    #facet_grid(perc) +
    coord_cartesian(xlim =c(11.7, 15), ylim = c(-.007, 0.02))+
    theme(legend.position = "none")+
    labs(x = "", y = ""),

  ggplot() +
    #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mrdoy_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
    geom_point(data = Wood_pheno_table_RP_mrdoy_hf, aes(x = climwinmean, y = max_rate_DOY)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    #facet_grid(perc) +
    coord_cartesian(xlim =c(12.3, 16), ylim = c(99, 235))+
    theme(legend.position = "none")+
    labs(x = "Temperature (c)", y = "DOY"),

  ggplot() +
    #geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mrdoy_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95)) +
    geom_point(data = Wood_pheno_table_DP_mrdoy_hf, aes(x = climwinmean, y = max_rate_DOY)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    #facet_grid(perc) +
    coord_cartesian(xlim =c(11.7, 15), ylim = c(99, 235))+
    theme(legend.position = "none")+
    labs(x = "Temperature (c)",y= ""),

  as.table = TRUE, nrow=5, ncol=4) ###as.table specifies order if multiple rows

dev.off()
