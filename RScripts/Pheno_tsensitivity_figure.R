# Setup ------------------------------------------------------------------------
#
# To get a quick overview of the sections of this code, go to RStudio menu bar ->
# Edit -> Folding -> Collapse all.
#

library(tidyverse)
library(lubridate)
library(tidybayes)
library(patchwork)
library(gridExtra)
library(knitr)
library(scales)
library(broom.mixed)
library(tictoc)

# rstanarm stuff
options(mc.cores = parallel::detectCores())
library(rstanarm)
library(patchwork)
# Number of MCMC chains & number of simulations per chain.
# Need to increase this at the end
n_iter <- 10000
n_chains <- 2

# Objects to keep during memory management clean-up
objects_to_keep <- c(
  "objects_to_keep", "n_iter", "n_chains",
  "fig6_RP", "fig6_DP", "fig6_RP_hf", "fig6_DP_hf",
  "fig6_RP_mrdoy", "fig6_DP_mrdoy", "fig6_RP_mrdoy_hf", "fig6_DP_mrdoy_hf",
  "fig6_RP_sl", "fig6_DP_sl", "fig6_RP_sl_hf", "fig6_DP_sl_hf",
  "fig6_RP_mr", "fig6_DP_mr", "fig6_RP_mr_hf", "fig6_DP_mr_hf",
  "fig6_RP_tot", "fig6_DP_tot", "fig6_RP_tot_hf", "fig6_DP_tot_hf",
  "woodtable", "woodtable_hf",
  "predictions_RP", "predictions_DP"
)

# Start timer
tic()



# 1. Run analysis on SCBI ---------------------------------------------------------
## Get growth data --------------------------------------------------------------
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_SCBI_CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # filter(tot >= 1) %>%
  # filter(tot <= 12.06)%>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

twofive <- subset(Wood_pheno_table, perc == .25)
fifty <- subset(Wood_pheno_table, perc == .5)
sevenfive <- subset(Wood_pheno_table, perc == .75)
# 25-50
twofifty <- cbind(twofive, fifty$DOY)
twofifty$twentyfive_to_fifty <- twofifty$`fifty$DOY` - twofifty$DOY
twofifty <- twofifty[, c(3, 7, 16)]
# 50-75
fiftyseventy <- cbind(fifty, sevenfive$DOY)
fiftyseventy$fifty_to_seventy <- fiftyseventy$`sevenfive$DOY` - fiftyseventy$DOY
fiftyseventy <- fiftyseventy[, c(3, 7, 16)]
# 25-75
twosevenfive <- cbind(twofive, sevenfive$DOY)
twosevenfive$seasonlength <- twosevenfive$`sevenfive$DOY` - twosevenfive$DOY
twosevenfive <- twosevenfive[, c(3, 7, 16)]


## Create temperature variables -------------------------------------------------
# 0. Get all weather data
#TMAX
weatherdata <-
  read_csv("climate data/met_tower_data_sensor2_ncdc_supplemented.csv") %>%
  filter(!is.na(cleantmax)) %>%
  mutate(year = year.x)

#TMIN
#weatherdata <-
#  read_csv("climate data/SCBI_tmin.csv") %>%
#  filter(!is.na(TMIN))

# 2. Get climwin data
#TMAX
climwindows <-
  read.csv("results/Climwin_results/Weekly/SCBI/weekly_climwin_results_SCBI_TMAX.csv") %>%
  filter(wood_type != "other") %>%
  mutate(
    #median_windowopendate = as.Date(median_windowopendate),
    #median_windowclosedate = as.Date(median_windowclosedate),
    #opendoy = yday(median_windowopendate),
    #closedoy = yday(median_windowclosedate)
    winopen = as.Date(refwoy * 7 - winopenwoy * 7, origin = paste0("2011-01-01")),
    winclose = as.Date(refwoy * 7 - winclosewoy * 7, origin = paste0("2011-01-01")),
    opendoy = yday(winopen),
    closedoy = yday(winclose)+7
  )
#climwindows$winopen <- as.Date(climwindows$refwoy * 7 - climwindows$winopenwoy * 7, origin = paste0("2011-01-01"))
#climwindows$winclose <- as.Date(climwindows$refwoy * 7 - climwindows$winclosewoy * 7, origin = paste0("2011-01-01"))

#TMIN
#climwindows <-
#  read.csv("results/Climwin_results/Weekly/SCBI/TMIN/weekly_climwin_results_SCBI_TMIN.csv") %>%
#  filter(wood_type != "other") %>%
#  mutate(
#    median_windowopendate = as.Date(median_windowopendate),
#    median_windowclosedate = as.Date(median_windowclosedate),
#    opendoy = yday(median_windowopendate),
#    closedoy = yday(median_windowclosedate)
#  )
#TMAX
climwinmeans_rp <- weatherdata %>%
  filter(doy %in% c(climwindows[1, 13]:climwindows[1, 14])) %>%
  group_by(year) %>%
  summarize(climwinmean = mean(cleantmax)) %>%
  mutate(wood_type = "ring-porous")

#TMIN
#climwinmeans_rp <- weatherdata %>%
#  filter(doy %in% c(climwindows[1, 11]:climwindows[1, 12])) %>%
#  group_by(year) %>%
#  summarize(climwinmean = mean(TMIN)) %>%
#  mutate(wood_type = "ring-porous")
#TMAX
climwinmeans_dp <- weatherdata %>%
  filter(doy %in% c(climwindows[4, 13]:climwindows[4, 14])) %>% # 68:135
  group_by(year) %>%
  summarize(climwinmean = mean(cleantmax)) %>%
  mutate(wood_type = "diffuse-porous")

#TMIN
#climwinmeans_dp <- weatherdata %>%
#  filter(doy %in% c(climwindows[4, 11]:climwindows[4, 12])) %>% # 68:135
#  group_by(year) %>%
#  summarize(climwinmean = mean(TMIN)) %>%
#  mutate(wood_type = "diffuse-porous")

# Combine
climwinmeans <- bind_rows(climwinmeans_rp, climwinmeans_dp)


# 3. Add to growth data
Wood_pheno_table <- Wood_pheno_table %>%
 # left_join(marchmeans, by = "year") %>%
  left_join(climwinmeans, by = c("year", "wood_type")) %>%
  left_join(twosevenfive, by = c("tag", "year")) %>%
  left_join(fiftyseventy, by = c("tag", "year")) %>%
  left_join(twofifty, by = c("tag", "year")) %>%
  # Remove other variables
  # select(-c(tot, dbh, max_rate_DOY, max_rate)) %>%
  mutate(
    perc = case_when(
      perc == 0.25 ~ "DOY_25",
      perc == 0.5 ~ "DOY_50",
      perc == 0.75 ~ "DOY_75"
    )
  ) %>%
  arrange(tag, year) %>%
  mutate(tag_year_perc = paste0(tag, year, perc))

# Pick out only distinct rows
unitag <- unique(Wood_pheno_table$tag_year_perc)
Wood_pheno_table <- distinct(Wood_pheno_table, tag_year_perc, .keep_all = TRUE)


### Model Fit 1 (multivariate): (DOY_25, DOY_50, DOY_75) using climwinmeans ----
# Convert to wide format for use in rstanarm::stan_mvmer()
Wood_pheno_table_wide <- Wood_pheno_table %>%
  select(perc, tag, year, wood_type, sp, climwinmean, starts_with("DOY")) %>%
  pivot_wider(names_from = perc, values_from = DOY)

# Fit multivariate model
joint_model_climwinmeans <- stan_mvmer(
  formula = list(
    DOY_25 ~ wood_type + wood_type:climwinmean + (1 | tag),
    DOY_50 ~ wood_type + wood_type:climwinmean + (1 | tag),
    DOY_75 ~ wood_type + wood_type:climwinmean + (1 | tag)
  ),
  # Note we transform the data from tall/tidy format to wide format first:
  data = Wood_pheno_table_wide,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

# Get regression table as output by rstanarm package, then clean. We will compare
# this table to posterior means of all fixed effects we compute later:
bayesian_regression_table <- joint_model_climwinmeans %>%
  summary() %>%
  as_tibble(rownames = "coefficient") %>%
  # Keep only relevant columns:
  select(coefficient, mean, sd, `2.5%`, `97.5%`) %>%
  # Keep only relevant rows:
  filter(!str_detect(coefficient, "Sigma")) %>%
  filter(str_detect(coefficient, "(Intercept)") | str_detect(coefficient, "wood_type"))
bayesian_regression_table
write.csv(bayesian_regression_table, file = "Results/Bayesian outputs/DOY_SCBI.csv", row.names = FALSE)
# Extract predicted DOY_25, DOY_50, DOY_75
# Note we need to take this long approach since tidybayes::add_predicted_draws()
# yields incorrected predicted/fitted values for stan_mvmer models as of 2020/11/18
# See https://github.com/mjskay/tidybayes/issues/271
y_hat <- c(
  joint_model_climwinmeans %>% posterior_predict(m = 1) %>% c(),
  joint_model_climwinmeans %>% posterior_predict(m = 2) %>% c(),
  joint_model_climwinmeans %>% posterior_predict(m = 3) %>% c()
)
predictions <- Wood_pheno_table %>%
  add_predicted_draws(joint_model_climwinmeans) %>%
  ungroup() %>%
  # Critical: sort by y outcome category (DOY_25, DOY_50, DOY_75) first
  arrange(perc, tag, year) %>%
  mutate(predictions_rstanarm = y_hat)

predictions_RP <- subset(predictions, wood_type == "ring-porous")
predictions_DP <- subset(predictions, wood_type == "diffuse-porous")
Wood_pheno_table_RP <- subset(Wood_pheno_table, wood_type == "ring-porous")
Wood_pheno_table_DP <- subset(Wood_pheno_table, wood_type == "diffuse-porous")
predictions_RP$sig <- ifelse(predictions_RP$perc == "DOY_75", 1, 0)

woodtable <- subset(Wood_pheno_table, perc == "DOY_25")

# Add max rate DOY
maxrateDOY_formulaRP <- "max_rate_DOY ~wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_maxrateDOY <- stan_lmer(
  formula = maxrateDOY_formulaRP,
  data = woodtable,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

MRDOY_scbi <- mixedmodel_stanlmerRP_maxrateDOY %>%
  tidy(conf.int = TRUE)
write.csv(MRDOY_scbi, file = "Results/Bayesian outputs/MRDOY_SCBI.csv", row.names = FALSE)

y_hat <- mixedmodel_stanlmerRP_maxrateDOY %>%
  posterior_predict() %>%
  c()

predictions_mrdoy <- woodtable %>%
  add_predicted_draws(mixedmodel_stanlmerRP_maxrateDOY) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hat)

predictions_mrdoy_RP <- subset(predictions_mrdoy, wood_type == "ring-porous")
predictions_mrdoy_RP$perc <- "Max Rate DOY"
predictions_mrdoy_RP$sig <- 0
predictions_RP <- rbind(predictions_RP, predictions_mrdoy_RP)
predictions_mrdoy_DP <- subset(predictions_mrdoy, wood_type == "diffuse-porous")
predictions_mrdoy_DP$perc <- "Max Rate DOY"
predictions_DP <- rbind(predictions_DP, predictions_mrdoy_DP)

Wood_pheno_table_RP_mrdoy <- subset(woodtable, wood_type == "ring-porous")
Wood_pheno_table_RP_mrdoy$perc <- "Max Rate DOY"
Wood_pheno_table_RP_mrdoy$DOY <- Wood_pheno_table_RP_mrdoy$max_rate_DOY
Wood_pheno_table_RP <- rbind(Wood_pheno_table_RP_mrdoy,Wood_pheno_table_RP)

Wood_pheno_table_DP_mrdoy <- subset(woodtable, wood_type == "diffuse-porous")
Wood_pheno_table_DP_mrdoy$perc <- "Max Rate DOY"
Wood_pheno_table_DP_mrdoy$DOY <- Wood_pheno_table_DP_mrdoy$max_rate_DOY
Wood_pheno_table_DP <- rbind(Wood_pheno_table_DP_mrdoy, Wood_pheno_table_DP)
#Wood_pheno_table_DP2 <- Wood_pheno_table_DP %>% mutate(perc = factor(perc, levels = c("Max Rate DOY", "DOY_25", "DOY_50", "DOY_75")))


fig6_RP <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_RP, aes(x = climwinmean, y = predictions_rstanarm, col = perc, fill = perc, linetype = as.factor(sig)), .width = .95, alpha = 0.5) +
  geom_point(data = Wood_pheno_table_RP, aes(x = climwinmean, y = DOY, col = perc)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_color_manual(values = c("red","dark green","blue", "purple"))+
  #scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(11.8, 19.2), ylim = c(min(Wood_pheno_table_RP$DOY)-3,max(Wood_pheno_table_RP$DOY)+3)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "DOY", col = "Percentile", title = "SCBI", subtitle = "Ring-porous")


fig6_DP <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_DP, aes(x = climwinmean, y = predictions_rstanarm, col = perc, fill = perc), .width = .95, alpha = 0.5, linetype = "solid") +
  geom_point(data = Wood_pheno_table_DP, aes(x = climwinmean, y = DOY, col = perc)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_color_manual(values = c("red","dark green","blue", "purple"))+
  #scale_fill_brewer() +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(14.7, 19.2), ylim = c(min(Wood_pheno_table_DP$DOY)-3,max(Wood_pheno_table_DP$DOY)+3)) +
  labs(x = "", y = "", col = "Percentile", title = "SCBI", subtitle = "Diffuse-porous")

# Clean-up
rm(list = setdiff(ls(), objects_to_keep))



### Model Fit 2: TOTAL GROWTH ----
total_formulaRP <- "dbh_total_growth ~ wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_total <- stan_lmer(
  formula = total_formulaRP,
  data = woodtable,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

tot_scbi <- mixedmodel_stanlmerRP_total %>%
  tidy(conf.int = TRUE)
write.csv(tot_scbi, file = "Results/Bayesian outputs/TOT_SCBI.csv", row.names = FALSE)
y_hot <- mixedmodel_stanlmerRP_total %>%
  posterior_predict() %>%
  c()

predictions_tot <- woodtable %>%
  add_predicted_draws(mixedmodel_stanlmerRP_total) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hot)

predictions_tot_RP <- subset(predictions_tot, wood_type == "ring-porous")
predictions_tot_DP <- subset(predictions_tot, wood_type == "diffuse-porous")
Wood_pheno_table_RP_tot <- subset(woodtable, wood_type == "ring-porous")
Wood_pheno_table_DP_tot <- subset(woodtable, wood_type == "diffuse-porous")

fig6_RP_tot <-  ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_tot_RP, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "dashed") +
  geom_point(data = Wood_pheno_table_RP_tot, aes(x = climwinmean, y = tot)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(11.8, 19.7), c(min(Wood_pheno_table_RP_tot$tot),max(Wood_pheno_table_RP_tot$tot))) +
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 20)) +
  labs(x =expression(paste("3/22-4/9 ", T[max], " (°C)")) , y = expression(paste(Delta * "DBH", " (cm)")))

fig6_DP_tot <-  ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_tot_DP, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "dashed") +
  geom_point(data = Wood_pheno_table_DP_tot, aes(x = climwinmean, y = tot)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(14.7, 19.2), ylim = c(min(Wood_pheno_table_DP_tot$tot),max(Wood_pheno_table_DP_tot$tot))) +
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 20)) +
  labs(x = expression(paste("2/19-5/9 ", T[max], " (°C)")), y = "")

# Clean-up
rm(list = setdiff(ls(), objects_to_keep))



### Model Fit 3: Season length ----
seasonlength_formulaRP <- "seasonlength ~ wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_seasonlength <- stan_lmer(
  formula = seasonlength_formulaRP,
  data = woodtable,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

SL_scbi <- mixedmodel_stanlmerRP_seasonlength %>%
  tidy(conf.int = TRUE)
write.csv(SL_scbi, file = "Results/Bayesian outputs/SL_SCBI.csv", row.names = FALSE)

y_hit <- mixedmodel_stanlmerRP_seasonlength %>%
  posterior_predict() %>%
  c()

predictions_sl <- woodtable %>%
  add_predicted_draws(mixedmodel_stanlmerRP_seasonlength) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hit)

predictions_sl_RP <- subset(predictions_sl, wood_type == "ring-porous")
predictions_sl_DP <- subset(predictions_sl, wood_type == "diffuse-porous")
Wood_pheno_table_RP_sl <- subset(woodtable, wood_type == "ring-porous")
Wood_pheno_table_DP_sl <- subset(woodtable, wood_type == "diffuse-porous")

fig6_RP_sl <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_sl_RP, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_RP_sl, aes(x = climwinmean, y = seasonlength)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(11.8, 19.7), ylim = c(min(Wood_pheno_table_RP_sl$seasonlength),95)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = expression(paste(L[pgs], " (days)")))

fig6_DP_sl <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_sl_DP, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_DP_sl, aes(x = climwinmean, y = seasonlength)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(14.7, 19.2), ylim = c(3, 90)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "")

# Clean-up
rm(list = setdiff(ls(), objects_to_keep))


### Model Fit 4: MAX RATE ----
maxrate_formulaRP <- "max_rate ~ wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_maxrate <- stan_lmer(
  formula = maxrate_formulaRP,
  data = woodtable,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

MR_scbi <- mixedmodel_stanlmerRP_maxrate %>%
  tidy(conf.int = TRUE)
write.csv(MR_scbi, file = "Results/Bayesian outputs/MR_SCBI.csv", row.names = FALSE)

y_het <- mixedmodel_stanlmerRP_maxrate %>%
  posterior_predict() %>%
  c()

predictions_mr <- woodtable %>%
  add_predicted_draws(mixedmodel_stanlmerRP_maxrate) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_het)

predictions_mr_RP <- subset(predictions_mr, wood_type == "ring-porous")
predictions_mr_DP <- subset(predictions_mr, wood_type == "diffuse-porous")
Wood_pheno_table_RP_mr <- subset(woodtable, wood_type == "ring-porous")
Wood_pheno_table_DP_mr <- subset(woodtable, wood_type == "diffuse-porous")

fig6_RP_mr <-  ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_mr_RP, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "dashed") +
  geom_point(data = Wood_pheno_table_RP_mr, aes(x = climwinmean, y = max_rate)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(11.8, 19.7), ylim = c(-.001, 0.015)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = expression(paste(g[max], " (cm/day)")))

fig6_DP_mr <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_mr_DP, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_DP_mr, aes(x = climwinmean, y = max_rate)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(14.7, 19.2), ylim = c(-.001, 0.015)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "")

# Clean-up
rm(list = setdiff(ls(), objects_to_keep))


### Model Fit 5: MAX RATE DOY ----
maxrateDOY_formulaRP <- "max_rate_DOY ~wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_maxrateDOY <- stan_lmer(
  formula = maxrateDOY_formulaRP,
  data = woodtable,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

MRDOY_scbi <- mixedmodel_stanlmerRP_maxrateDOY %>%
  tidy(conf.int = TRUE)
write.csv(MRDOY_scbi, file = "Results/Bayesian outputs/MRDOY_SCBI.csv", row.names = FALSE)

y_hat <- mixedmodel_stanlmerRP_maxrateDOY %>%
  posterior_predict() %>%
  c()

predictions_mrdoy <- woodtable %>%
  add_predicted_draws(mixedmodel_stanlmerRP_maxrateDOY) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hat)

predictions_mrdoy_RP <- subset(predictions_mrdoy, wood_type == "ring-porous")
predictions_mrdoy_RP$perc <- "Max Rate DOY"
predictions_mrdoy_RP$sig <- 0
predictions_RP <- rbind(predictions_RP, predictions_mrdoy_RP)
predictions_mrdoy_DP <- subset(predictions_mrdoy, wood_type == "diffuse-porous")
predictions_mrdoy_DP$perc <- "Max Rate DOY"
predictions_DP <- rbind(predictions_DP, predictions_mrdoy_DP)

Wood_pheno_table_RP_mrdoy <- subset(woodtable, wood_type == "ring-porous")
Wood_pheno_table_DP_mrdoy <- subset(woodtable, wood_type == "diffuse-porous")

fig6_RP_mrdoy <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_mrdoy_RP, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_RP_mrdoy, aes(x = climwinmean, y = max_rate_DOY)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(11.8, 19.7), ylim = c(110, 200)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = expression(DOY[g[max]]))

fig6_DP_mrdoy <-  ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_mrdoy_DP, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_DP_mrdoy, aes(x = climwinmean, y = max_rate_DOY)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(14.7, 19.2), ylim = c(133, 210)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "")

# Clean-up
rm(list = setdiff(ls(), objects_to_keep))





# 2. Run analysis on Harvard Forest -----------------------------------------------
## Get growth data --------------------------------------------------------------
Wood_pheno_table_hf <- read_csv("Data/Wood_pheno_table_HarvardForest_CLEAN.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # filter(tot >= 1) %>%
  # filter(tot <= 12.06)%>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))
# Wood_pheno_table$tag <- substr(Wood_pheno_table$tag,1, nchar(as.character(Wood_pheno_table$tag))-4)

twofive_hf <- subset(Wood_pheno_table_hf, perc == .25)
fifty_hf <- subset(Wood_pheno_table_hf, perc == .5)
sevenfive_hf <- subset(Wood_pheno_table_hf, perc == .75)
# 25-50
twofifty_hf <- cbind(twofive_hf, fifty_hf$DOY)
twofifty_hf$twentyfive_to_fifty <- twofifty_hf$`fifty_hf$DOY` - twofifty_hf$DOY
twofifty_hf <- twofifty_hf[, c(3, 6, 17)]
# 50-75
fiftyseventy_hf <- cbind(fifty_hf, sevenfive_hf$DOY)
fiftyseventy_hf$fifty_to_seventy <- fiftyseventy_hf$`sevenfive_hf$DOY` - fiftyseventy_hf$DOY
fiftyseventy_hf <- fiftyseventy_hf[, c(3, 6, 17)]
# 25-75
twosevenfive_hf <- cbind(twofive_hf, sevenfive_hf$DOY)
twosevenfive_hf$seasonlength <- twosevenfive_hf$`sevenfive_hf$DOY` - twosevenfive_hf$DOY
twosevenfive_hf <- twosevenfive_hf[, c(3, 6, 17)]


## Create temperature variables -------------------------------------------------
# 0. Get all weather data
#TMAX
weatherdata_hf <-
  read_csv("climate data/HF_weatherdata.csv") %>%
  filter(!is.na(airtmax))

#TMIN
#weatherdata_hf <-
#  read_csv("climate data/HF_weatherdata_TMIN.csv") %>%
#  filter(!is.na(airtmin))

# 1. Get mean march daily maximum temperatures
#marchmeans_hf <- weatherdata_hf %>%
#  filter(month == 3) %>%
#  group_by(year) %>%
#  summarize(marchmean = mean(airtmax))

# 2. Get climwin data
#TMAX
climwindows_hf <-
  read.csv("results/Climwin_results/Weekly/Harvard Forest/weekly_climwin_results_HF_TMAX.csv") %>%
  filter(wood_type != "other") %>%
  mutate(
    #median_windowopendate = as.Date(median_windowopendate, format = "%Y-%m-%d"),
    #median_windowclosedate = as.Date(median_windowclosedate, format = "%Y-%m-%d"),
    #opendoy = yday(median_windowopendate),
    #closedoy = yday(median_windowclosedate)
    winopen = as.Date(refwoy * 7 - winopenwoy * 7, origin = paste0("2011-01-01")),
    winclose = as.Date(refwoy * 7 - winclosewoy * 7, origin = paste0("2011-01-01")),
    opendoy = yday(winopen),
    closedoy = yday(winclose)+7
  )

#TMIN
#climwindows_hf <-
#  read.csv("results/Climwin_results/Weekly/Harvard Forest/TMIN/weekly_climwin_results_HF_TMIN.csv") %>%
#  filter(wood_type != "other") %>%
#  mutate(
#    median_windowopendate = as.Date(median_windowopendate, format = "%Y-%m-%d"),
#    median_windowclosedate = as.Date(median_windowclosedate, format = "%Y-%m-%d"),
#    opendoy = yday(median_windowopendate),
#    closedoy = yday(median_windowclosedate)
#  )


#TMAX
climwinmeans_rp_hf <- weatherdata_hf %>%
  filter(DOY %in% c(climwindows_hf[4, 13]:climwindows_hf[4, 14])) %>%
  group_by(year) %>%
  summarize(climwinmean = mean(airtmax)) %>%
  mutate(wood_type = "ring-porous")

#TMIN
#climwinmeans_rp_hf <- weatherdata_hf %>%
#  filter(DOY %in% c(climwindows_hf[4, 11]:climwindows_hf[4, 12])) %>%
#  group_by(year) %>%
#  summarize(climwinmean = mean(airtmin)) %>%
#  mutate(wood_type = "ring-porous")

#TMAX
climwinmeans_dp_hf <- weatherdata_hf %>%
  filter(DOY %in% c(climwindows_hf[1, 13]:climwindows_hf[1, 14])) %>% # 68:135
  group_by(year) %>%
  summarize(climwinmean = mean(airtmax)) %>%
  mutate(wood_type = "diffuse-porous")

#TMIN
#climwinmeans_dp_hf <- weatherdata_hf %>%
#  filter(DOY %in% c(climwindows_hf[1, 11]:climwindows_hf[1, 12])) %>% # 68:135
#  group_by(year) %>%
#  summarize(climwinmean = mean(airtmin)) %>%
#  mutate(wood_type = "diffuse-porous")

# Combine
climwinmeans_hf <- bind_rows(climwinmeans_rp_hf, climwinmeans_dp_hf)


# 3. Add to growth data
Wood_pheno_table_hf <- Wood_pheno_table_hf %>%
#  left_join(marchmeans_hf, by = "year") %>%
  left_join(climwinmeans_hf, by = c("year", "wood_type")) %>%
  left_join(twosevenfive_hf, by = c("tag", "year")) %>%
  left_join(fiftyseventy_hf, by = c("tag", "year")) %>%
  left_join(twofifty_hf, by = c("tag", "year")) %>%
  # Remove other variables
  # select(-c(tot, dbh, max_rate_DOY, max_rate)) %>%
  mutate(
    perc = case_when(
      perc == 0.25 ~ "DOY_25",
      perc == 0.5 ~ "DOY_50",
      perc == 0.75 ~ "DOY_75"
    )
  ) %>%
  arrange(tag, year)

### Model Fit 1 (multivariate): (DOY_25, DOY_50, DOY_75) using climwinmeans ----
# Convert to wide format for use in rstanarm::stan_mvmer()
Wood_pheno_table_wide_hf <- Wood_pheno_table_hf %>%
  select(perc, tag, year, wood_type, sp, climwinmean, starts_with("DOY"), site) %>%
  pivot_wider(names_from = perc, values_from = DOY)

# Fit multivariate model
joint_model_climwinmeans_hf <- stan_mvmer(
  formula = list(
    DOY_25 ~ wood_type + wood_type:climwinmean + (1 | site) + (1 | tag),
    DOY_50 ~ wood_type + wood_type:climwinmean + (1 | site) + (1 | tag),
    DOY_75 ~ wood_type + wood_type:climwinmean + (1 | site) + (1 | tag)
  ),
  # Note we transform the data from tall/tidy format to wide format first:
  data = Wood_pheno_table_wide_hf,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

bayesian_regression_table_hf <- joint_model_climwinmeans_hf %>%
  summary() %>%
  as_tibble(rownames = "coefficient") %>%
  # Keep only relevant columns:
  select(coefficient, mean, sd, `2.5%`, `97.5%`) %>%
  # Keep only relevant rows:
  filter(!str_detect(coefficient, "Sigma")) %>%
  filter(str_detect(coefficient, "(Intercept)") | str_detect(coefficient, "wood_type"))
bayesian_regression_table_hf
write.csv(bayesian_regression_table_hf, file = "Results/Bayesian outputs/DOY_HF.csv", row.names = FALSE)

# Extract predicted DOY_25, DOY_50, DOY_75
# Note we need to take this long approach since tidybayes::add_predicted_draws()
# yields incorrected predicted/fitted values for stan_mvmer models as of 2020/11/18
# See https://github.com/mjskay/tidybayes/issues/271
y_hat_hf <- c(
  joint_model_climwinmeans_hf %>% posterior_predict(m = 1) %>% c(),
  joint_model_climwinmeans_hf %>% posterior_predict(m = 2) %>% c(),
  joint_model_climwinmeans_hf %>% posterior_predict(m = 3) %>% c()
)
predictions_hf <- Wood_pheno_table_hf %>%
  add_predicted_draws(joint_model_climwinmeans_hf) %>%
  ungroup() %>%
  # Critical: sort by y outcome category (DOY_25, DOY_50, DOY_75) first
  arrange(perc, tag, year) %>%
  mutate(predictions_rstanarm = y_hat_hf)

predictions_RP_hf <- subset(predictions_hf, wood_type == "ring-porous")
predictions_DP_hf <- subset(predictions_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_hf <- subset(Wood_pheno_table_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_hf <- subset(Wood_pheno_table_hf, wood_type == "diffuse-porous")

woodtable_hf <- subset(Wood_pheno_table_hf, perc == "DOY_25")

# Add max rate DOY
maxrateDOY_formulaRP <- "max_rate_DOY ~wood_type + wood_type:climwinmean + (1|site) + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_maxrateDOY_hf <- stan_lmer(
  formula = maxrateDOY_formulaRP,
  data = woodtable_hf,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

MRDOY_hf <- mixedmodel_stanlmerRP_maxrateDOY_hf %>%
  tidy(conf.int = TRUE)
write.csv(MRDOY_hf, file = "Results/Bayesian outputs/MRDOY_HF.csv", row.names = FALSE)

y_hat_hf <- mixedmodel_stanlmerRP_maxrateDOY_hf %>%
  posterior_predict() %>%
  c()

predictions_mrdoy_hf <- woodtable_hf %>%
  add_predicted_draws(mixedmodel_stanlmerRP_maxrateDOY_hf) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hat_hf)

predictions_mrdoy_RP_hf <- subset(predictions_mrdoy_hf, wood_type == "ring-porous")
predictions_mrdoy_DP_hf <- subset(predictions_mrdoy_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_mrdoy_hf <- subset(woodtable_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_mrdoy_hf <- subset(woodtable_hf, wood_type == "diffuse-porous")

predictions_mrdoy_RP_hf <- subset(predictions_mrdoy_hf, wood_type == "ring-porous")
predictions_mrdoy_RP_hf$perc <- "Max Rate DOY"
#predictions_mrdoy_RP_hf$sig <- 0
predictions_RP_hf <- rbind(predictions_RP_hf, predictions_mrdoy_RP_hf)
predictions_mrdoy_DP_hf <- subset(predictions_mrdoy_hf, wood_type == "diffuse-porous")
predictions_mrdoy_DP_hf$perc <- "Max Rate DOY"
predictions_DP_hf <- rbind(predictions_DP_hf, predictions_mrdoy_DP_hf)

Wood_pheno_table_RP_mrdoy_hf <- subset(woodtable_hf, wood_type == "ring-porous")
Wood_pheno_table_RP_mrdoy_hf$perc <- "Max Rate DOY"
Wood_pheno_table_RP_mrdoy_hf$DOY <- Wood_pheno_table_RP_mrdoy_hf$max_rate_DOY
Wood_pheno_table_RP_hf <- rbind(Wood_pheno_table_RP_mrdoy_hf,Wood_pheno_table_RP_hf)

Wood_pheno_table_DP_mrdoy_hf <- subset(woodtable_hf, wood_type == "diffuse-porous")
Wood_pheno_table_DP_mrdoy_hf$perc <- "Max Rate DOY"
Wood_pheno_table_DP_mrdoy_hf$DOY <- Wood_pheno_table_DP_mrdoy_hf$max_rate_DOY
Wood_pheno_table_DP_hf <- rbind(Wood_pheno_table_DP_mrdoy_hf,Wood_pheno_table_DP_hf)

fig6_RP_hf <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_RP_hf, aes(x = climwinmean, y = predictions_rstanarm, fill = perc, col = perc), .width = .95, alpha = 0.5, linetype = "solid") +
  geom_point(data = Wood_pheno_table_RP_hf, aes(x = climwinmean, y = DOY, col = perc)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_color_manual(values = c("red","dark green","blue","purple"))+
  #scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(12.3, 16), ylim = c(min(Wood_pheno_table_RP_hf$DOY), max(Wood_pheno_table_RP_hf$DOY))) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "", col = "Percentile", title = "Harvard Forest", subtitle = "Ring-porous")
# geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")

fig6_DP_hf <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_DP_hf, aes(x = climwinmean, y = predictions_rstanarm, fill = perc, col = perc), .width = .95, alpha = 0.5, linetype = "solid",show.legend = FALSE) +
  geom_point(data = Wood_pheno_table_DP_hf, aes(x = climwinmean, y = DOY, col = perc)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_color_manual(values = c("red","dark green","blue","purple"),  labels = expression(DOY[25], DOY[50],DOY[75],DOY[g[max]]))+
  #scale_fill_brewer() +
  theme_bw()+
  theme(legend.position = c(.86, .5),
        legend.text.align = 0,
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(11.7, 15.5), ylim = c(min(Wood_pheno_table_DP_hf$DOY), max(Wood_pheno_table_DP_hf$DOY))) +
  labs(x = "", y = "", fill = "Percentile", col = "Variable", title = "Harvard Forest", subtitle = "Diffuse-porous")

# Clean-up
rm(list = setdiff(ls(), objects_to_keep))


### Model Fit 2: TOTAL GROWTH ----
total_formulaRP <- "dbh_total_growth ~ wood_type + wood_type:climwinmean + (1|site) + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_total_hf <- stan_lmer(
  formula = total_formulaRP,
  data = woodtable_hf,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

tot_hf <- mixedmodel_stanlmerRP_total_hf %>%
  tidy(conf.int = TRUE)
write.csv(tot_hf, file = "Results/Bayesian outputs/TOT_HF.csv", row.names = FALSE)

y_hot_hf <- mixedmodel_stanlmerRP_total_hf %>%
  posterior_predict() %>%
  c()

predictions_tot_hf <- woodtable_hf %>%
  add_predicted_draws(mixedmodel_stanlmerRP_total_hf) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hot_hf)

predictions_tot_RP_hf <- subset(predictions_tot_hf, wood_type == "ring-porous")
predictions_tot_DP_hf <- subset(predictions_tot_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_tot_hf <- subset(woodtable_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_tot_hf <- subset(woodtable_hf, wood_type == "diffuse-porous")

fig6_RP_tot_hf <-   ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_tot_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_RP_tot_hf, aes(x = climwinmean, y = tot)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(12.3, 16), ylim = c(-.1, 1.20)) +
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 20)) +
  labs(x = expression(paste("4/2-5/7 ", T[max], " (°C)")), y = "")

fig6_DP_tot_hf <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_tot_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_DP_tot_hf, aes(x = climwinmean, y = tot)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(11.7, 15.5), ylim = c(-.15, 1)) +
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 20)) +
  labs(x = expression(paste("3/19-5/7 ", T[max], " (°C)")), y = "")

# Clean-up
rm(list = setdiff(ls(), objects_to_keep))


### Model Fit 3: Season length ----
seasonlength_formulaRP <- "seasonlength ~ wood_type + wood_type:climwinmean + (1|site) + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_seasonlength_hf <- stan_lmer(
  formula = seasonlength_formulaRP,
  data = woodtable_hf,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

SL_hf <- mixedmodel_stanlmerRP_seasonlength_hf %>%
  tidy(conf.int = TRUE)
write.csv(SL_hf, file = "Results/Bayesian outputs/SL_HF.csv", row.names = FALSE)

y_hit_hf <- mixedmodel_stanlmerRP_seasonlength_hf %>%
  posterior_predict() %>%
  c()

predictions_sl_hf <- woodtable_hf %>%
  add_predicted_draws(mixedmodel_stanlmerRP_seasonlength_hf) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hit_hf)

predictions_sl_RP_hf <- subset(predictions_sl_hf, wood_type == "ring-porous")
predictions_sl_DP_hf <- subset(predictions_sl_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_sl_hf <- subset(woodtable_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_sl_hf <- subset(woodtable_hf, wood_type == "diffuse-porous")

fig6_RP_sl_hf <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_sl_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_RP_sl_hf, aes(x = climwinmean, y = seasonlength)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(12.3, 16), ylim = c(15, 100)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "")

fig6_DP_sl_hf <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_sl_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_DP_sl_hf, aes(x = climwinmean, y = seasonlength)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(11.7, 15.5), ylim = c(3, 100)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "")

# Clean-up
rm(list = setdiff(ls(), objects_to_keep))


### Model Fit 4: MAX RATE ----
maxrate_formulaRP <- "max_rate ~ wood_type + wood_type:climwinmean + (1|site) + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_maxrate_hf <- stan_lmer(
  formula = maxrate_formulaRP,
  data = woodtable_hf,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

MR_hf <- mixedmodel_stanlmerRP_maxrate_hf %>%
  tidy(conf.int = TRUE)
write.csv(MR_hf, file = "Results/Bayesian outputs/MR_HF.csv", row.names = FALSE)

y_het_hf <- mixedmodel_stanlmerRP_maxrate_hf %>%
  posterior_predict() %>%
  c()

predictions_mr_hf <- woodtable_hf %>%
  add_predicted_draws(mixedmodel_stanlmerRP_maxrate_hf) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_het_hf)

predictions_mr_RP_hf <- subset(predictions_mr_hf, wood_type == "ring-porous")
predictions_mr_DP_hf <- subset(predictions_mr_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_mr_hf <- subset(woodtable_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_mr_hf <- subset(woodtable_hf, wood_type == "diffuse-porous")

fig6_RP_mr_hf <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_mr_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_RP_mr_hf, aes(x = climwinmean, y = max_rate)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(12.3, 16), ylim = c(-.001, 0.01)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "")

fig6_DP_mr_hf <-   ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_mr_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_DP_mr_hf, aes(x = climwinmean, y = max_rate)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(11.7, 15.5), ylim = c(-.001, 0.01)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "")

# Clean-up
rm(list = setdiff(ls(), objects_to_keep))


### Model Fit 5: MAX RATE DOY ----
maxrateDOY_formulaRP <- "max_rate_DOY ~wood_type + wood_type:climwinmean + (1|site) + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_maxrateDOY_hf <- stan_lmer(
  formula = maxrateDOY_formulaRP,
  data = woodtable_hf,
  seed = 349,
  iter = n_iter,
  chains = n_chains
)

MRDOY_hf <- mixedmodel_stanlmerRP_maxrateDOY_hf %>%
  tidy(conf.int = TRUE)
write.csv(MRDOY_hf, file = "Results/Bayesian outputs/MRDOY_HF.csv", row.names = FALSE)

y_hat_hf <- mixedmodel_stanlmerRP_maxrateDOY_hf %>%
  posterior_predict() %>%
  c()

predictions_mrdoy_hf <- woodtable_hf %>%
  add_predicted_draws(mixedmodel_stanlmerRP_maxrateDOY_hf) %>%
  ungroup() %>%
  arrange(tag, year) %>%
  mutate(predictions_rstanarm = y_hat_hf)

predictions_mrdoy_RP_hf <- subset(predictions_mrdoy_hf, wood_type == "ring-porous")
predictions_mrdoy_DP_hf <- subset(predictions_mrdoy_hf, wood_type == "diffuse-porous")
Wood_pheno_table_RP_mrdoy_hf <- subset(woodtable_hf, wood_type == "ring-porous")
Wood_pheno_table_DP_mrdoy_hf <- subset(woodtable_hf, wood_type == "diffuse-porous")

fig6_RP_mrdoy_hf <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_mrdoy_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_RP_mrdoy_hf, aes(x = climwinmean, y = max_rate_DOY)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(12.3, 16), ylim = c(110, 210)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "")

fig6_DP_mrdoy_hf <- ggplot() +
  # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions_mrdoy_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
  geom_point(data = Wood_pheno_table_DP_mrdoy_hf, aes(x = climwinmean, y = max_rate_DOY)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  # facet_grid(perc) +
  coord_cartesian(xlim = c(11.7, 15.5), ylim = c(120, 235)) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        text = element_text(size = 20)) +
  labs(x = "", y = "")

# Clean-up
rm(list = setdiff(ls(), objects_to_keep))


# 3. Save and combine model fit plots for SCBI and Harvard Forest together -----------------------------------------
# End timer
timer <- toc()
timer$toc - timer$tic

## Save all figure objects ------------
# save(list = objects_to_keep, file = "doc/manuscript/tables_figures/figures.RData")

## Create single figure using patchwork ----------
png(
  filename = "doc/manuscript/tables_figures/pheno_Tsensitivity_combo_patchwork.png", width = 24, height = 20,
  pointsize = 12, bg = "transparent", units = "in", res = 600
  #restoreConsole = FALSE
)
# DOY:
fig6_RP + fig6_DP + fig6_RP_hf + fig6_DP_hf +
  # Max rate DOY:
  #fig6_RP_mrdoy + fig6_DP_mrdoy + fig6_RP_mrdoy_hf + fig6_DP_mrdoy_hf +
  # Season length:
  fig6_RP_sl + fig6_DP_sl + fig6_RP_sl_hf + fig6_DP_sl_hf +
  # Maximum growth rate:
  fig6_RP_mr + fig6_DP_mr + fig6_RP_mr_hf + fig6_DP_mr_hf +
  # Total growth:
  fig6_RP_tot + fig6_DP_tot + fig6_RP_tot_hf + fig6_DP_tot_hf +
  plot_layout(nrow = 4)
dev.off()

## Create single figure using grid.arrange ----------
png(
  filename = "doc/manuscript/tables_figures/pheno_Tsensitivity_combo.png", width = 15, height = 25,
  pointsize = 12, bg = "transparent", units = "in", res = 600,
  restoreConsole = FALSE
)
grid.arrange(
  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_RP, aes(x = climwinmean, y = predictions_rstanarm, group = perc, col = perc), .width = .95) +
    geom_point(data = Wood_pheno_table_RP, aes(x = climwinmean, y = DOY, col = perc)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(11.8, 19.7), ylim = c(80, 240)) +
    theme(legend.position = "none") +
    labs(x = "", y = "DOY", col = "Percentile", title = "SCBI", subtitle = "Ring-porous"),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_DP, aes(x = climwinmean, y = predictions_rstanarm, group = perc, col = perc), .width = .95, linetype = "solid") +
    geom_point(data = Wood_pheno_table_DP, aes(x = climwinmean, y = DOY, col = perc)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    theme(legend.position = "none") +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(14.7, 19.2), ylim = c(80, 240)) +
    labs(x = "", y = "", col = "Percentile", title = "SCBI", subtitle = "Diffuse-porous"),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_RP_hf, aes(x = climwinmean, y = predictions_rstanarm, group = perc, col = perc), .width = .95, linetype = "solid") +
    geom_point(data = Wood_pheno_table_RP_hf, aes(x = climwinmean, y = DOY, col = perc)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(12.3, 16), ylim = c(44, 261)) +
    theme(legend.position = "none") +
    labs(x = "", y = "", col = "Percentile", title = "Harvard Forest", subtitle = "Ring-porous"),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_DP_hf, aes(x = climwinmean, y = predictions_rstanarm, group = perc, col = perc), .width = .95, linetype = "solid") +
    geom_point(data = Wood_pheno_table_DP_hf, aes(x = climwinmean, y = DOY, col = perc)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    theme(legend.position = c(.95, .5)) +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(11.7, 15.5), ylim = c(80, 240)) +
    labs(x = "", y = "", col = "Percentile", title = "Harvard Forest", subtitle = "Diffuse-porous"),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mrdoy_RP, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
    geom_point(data = Wood_pheno_table_RP_mrdoy, aes(x = climwinmean, y = max_rate_DOY)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(11.8, 19.7), ylim = c(99, 235)) +
    theme(legend.position = "none") +
    labs(x = "", y = "Max Rate DOY"),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mrdoy_DP, aes(x = climwinmean, y = predictions_rstanarm), .width = .95, linetype = "solid") +
    geom_point(data = Wood_pheno_table_DP_mrdoy, aes(x = climwinmean, y = max_rate_DOY)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(14.7, 19.2), ylim = c(99, 235)) +
    theme(legend.position = "none") +
    labs(x = "", y = ""),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mrdoy_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "solid") +
    geom_point(data = Wood_pheno_table_RP_mrdoy_hf, aes(x = climwinmean, y = max_rate_DOY)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(12.3, 16), ylim = c(99, 235)) +
    theme(legend.position = "none") +
    labs(x = "", y = "DOY"),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mrdoy_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "solid") +
    geom_point(data = Wood_pheno_table_DP_mrdoy_hf, aes(x = climwinmean, y = max_rate_DOY)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(11.7, 15), ylim = c(99, 235)) +
    theme(legend.position = "none") +
    labs(x = "", y = ""),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_sl_RP, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "solid") +
    geom_point(data = Wood_pheno_table_RP_sl, aes(x = climwinmean, y = seasonlength)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(11.8, 19.7), ylim = c(3, 106)) +
    theme(legend.position = "none") +
    labs(x = "", y = "Season Length (# of Days)"),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_sl_DP, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "dashed") +
    geom_point(data = Wood_pheno_table_DP_sl, aes(x = climwinmean, y = seasonlength)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(14.7, 19.2), ylim = c(3, 90)) +
    theme(legend.position = "none") +
    labs(x = "", y = ""),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_sl_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "solid") +
    geom_point(data = Wood_pheno_table_RP_sl_hf, aes(x = climwinmean, y = seasonlength)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(12.3, 16), ylim = c(3, 106)) +
    theme(legend.position = "none") +
    labs(x = "", y = ""),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_sl_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "solid") +
    geom_point(data = Wood_pheno_table_DP_sl_hf, aes(x = climwinmean, y = seasonlength)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(11.7, 15), ylim = c(3, 106)) +
    theme(legend.position = "none") +
    labs(x = "", y = ""),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mr_RP, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "dashed") +
    geom_point(data = Wood_pheno_table_RP_mr, aes(x = climwinmean, y = max_rate)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(11.8, 19.7), ylim = c(-.007, 0.02)) +
    theme(legend.position = "none") +
    labs(x = "", y = "Maximum Growth Rate (cm/day)"),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mr_DP, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "dashed") +
    geom_point(data = Wood_pheno_table_DP_mr, aes(x = climwinmean, y = max_rate)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(14.7, 19.2), ylim = c(-.007, 0.02)) +
    theme(legend.position = "none") +
    labs(x = "", y = ""),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mr_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "solid") +
    geom_point(data = Wood_pheno_table_RP_mr_hf, aes(x = climwinmean, y = max_rate)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(12.3, 16), ylim = c(-.007, 0.02)) +
    theme(legend.position = "none") +
    labs(x = "", y = ""),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_mr_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "solid") +
    geom_point(data = Wood_pheno_table_DP_mr_hf, aes(x = climwinmean, y = max_rate)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(11.7, 15), ylim = c(-.007, 0.02)) +
    theme(legend.position = "none") +
    labs(x = "", y = ""),


  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_tot_RP, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "dashed") +
    geom_point(data = Wood_pheno_table_RP_tot, aes(x = climwinmean, y = tot)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(11.8, 19.7), ylim = c(-.4, 1.5)) +
    theme(legend.position = "none") +
    labs(x = "Temperature (c) 3/22-4/9", y = "Total Growth (cm)"),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_tot_DP, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "dashed") +
    geom_point(data = Wood_pheno_table_DP_tot, aes(x = climwinmean, y = tot)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(14.7, 19.2), ylim = c(-.4, 1.5)) +
    theme(legend.position = "none") +
    labs(x = "Temperature (c) 2/19-5/21", y = ""),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_tot_RP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "solid") +
    geom_point(data = Wood_pheno_table_RP_tot_hf, aes(x = climwinmean, y = tot)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(12.3, 16), ylim = c(-.27, 1.28)) +
    theme(legend.position = "none") +
    labs(x = "Temperature (c) 4/2-5/7", y = ""),

  ggplot() +
    # geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
    stat_lineribbon(data = predictions_tot_DP_hf, aes(x = climwinmean, y = predictions_rstanarm), .width = c(.99, .95), linetype = "solid") +
    geom_point(data = Wood_pheno_table_DP_tot_hf, aes(x = climwinmean, y = tot)) +
    # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
    scale_fill_brewer() +
    # facet_grid(perc) +
    coord_cartesian(xlim = c(11.7, 15), ylim = c(-.27, 1)) +
    theme(legend.position = "none") +
    labs(x = "Temperature (c) 3/19-5/7", y = ""),
  as.table = TRUE, nrow = 5, ncol = 4
) ### as.table specifies order if multiple rows

dev.off()
