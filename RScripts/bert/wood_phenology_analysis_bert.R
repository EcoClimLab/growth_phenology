# Load packages and data ---------------------------------------
library(tidyverse)
library(lubridate)
library(tidybayes)
library(patchwork)
library(knitr)
library(rstanarm)
options(mc.cores = parallel::detectCores())



# Get growth data ----------------------------------
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V4.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))




# Create temperature variables ----------------------------------
# 0. Get all weather data
weatherdata <-
  read_csv("climate data/NCDC_NOAA_precip_temp.csv") %>%
  mutate(
    DATE = dmy(DATE),
    months = month(DATE, label = TRUE, abbr = FALSE)
  ) %>%
  # Remove entries with no tmax data
  filter(!is.na(TMAX)) %>%
  # Rename RP flag set by Cam
  rename(flagrp = flag)


# 1. Get mean march daily maximum temperatures
marchmeans <- weatherdata %>%
  filter(months == "March") %>%
  group_by(year) %>%
  summarize(marchmean = mean(TMAX))


# 2.a) EDA of climwin windows
# RP climwin window is around 3/15 to 4/23
weatherdata %>%
  filter(flagrp == "RP") %>%
  mutate(DOY = yday(DATE)) %>%
  arrange(DOY) %>%
  slice(c(1, n()))

# DP climwin window is around 3/27 to 6/2
weatherdata %>%
  filter(flagdp == "DP") %>%
  mutate(DOY = yday(DATE)) %>%
  arrange(DOY) %>%
  slice(c(1, n()))

climwin_windows <-
  tibble(
    wood_type = c("diffuse-porous", "ring-porous"),
    window = c("climwin window: 3/27 - 6/2", "climwin window: 3/15 - 4/23")
  )


# 2.b) Get mean climwin daily maximum temperatures
# RP separately
climwinmeans_rp <- weatherdata %>%
  filter(flagrp == "RP") %>%
  group_by(year) %>%
  summarize(climwinmean = mean(TMAX)) %>%
  mutate(wood_type = "ring-porous")

# DP separately
climwinmeans_dp <- weatherdata %>%
  filter(flagdp == "DP") %>%
  group_by(year) %>%
  summarize(climwinmean = mean(TMAX)) %>%
  mutate(wood_type = "diffuse-porous")

# Combine
climwinmeans <- bind_rows(climwinmeans_rp, climwinmeans_dp)


# 3. Add to growth data
Wood_pheno_table <- Wood_pheno_table %>%
  left_join(marchmeans, by = "year") %>%
  left_join(climwinmeans, by = c("year", "wood_type")) %>%
  # Remove other variables
  select(-c(tot, dbh, max_rate_DOY, max_rate)) %>%
  mutate(
    perc = case_when(
      perc == 0.25 ~ "DOY_25",
      perc == 0.5 ~ "DOY_50",
      perc == 0.75 ~ "DOY_75"
    )
  ) %>%
  arrange(tag, year)
View(Wood_pheno_table)


# TODO Erase later: Run analysis
# - Only for subset of tags to speed up computation
# - Recenter all climwin values at 65 since mean of climwinmeans is 66.310
climwin_mean <- mean(climwinmeans$climwinmean)
climwin_mean

set.seed(76)
sample_tags <- Wood_pheno_table %>%
  pull(tag) %>%
  unique() %>%
  sample(20)

Wood_pheno_table <- Wood_pheno_table %>%
  filter(tag %in% sample_tags) %>%
  mutate(climwinmean = climwinmean - 65)




# Fit multivariate model using climwinmeans ------------------------------------
Wood_pheno_table <- Wood_pheno_table %>%
  select(perc, tag, year, wood_type, sp, climwinmean, starts_with("DOY"))

# Convert to wide format for use in rstanarm::stan_mvmer()
Wood_pheno_table_wide <- Wood_pheno_table %>%
  pivot_wider(names_from = perc, values_from = DOY)

# Fit multivariate model
joint_model_climwinmeans <- stan_mvmer(
  formula = list(
    DOY_25 ~ wood_type + wood_type:climwinmean + (1|tag),
    DOY_50 ~ wood_type + wood_type:climwinmean + (1|tag),
    DOY_75 ~ wood_type + wood_type:climwinmean + (1|tag)
  ),
  # Note we transform the data from tall/tidy format to wide format first:
  data = Wood_pheno_table_wide,
  seed = 76,
  # Once we feel good about our results, increase these values. The code will
  # take longer to run however
  chains = 2,
  iter = 2000
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




# Extract all posterior draws used for all Bayesian inference ------------------
# Identify all paramters: Anything with a
# - b[] is a random effect
# - Sigma is a variance parameter
joint_model_climwinmeans %>%
  get_variables()

# Extract posterior draws
posterior_draws <- joint_model_climwinmeans %>%
  # This is a hack done manually. There must be a regex way to do this:
  gather_draws(
    # Fixed effects:
    `y1|(Intercept)`, `y2|(Intercept)`, `y3|(Intercept)`,
    `y1|wood_typering-porous`, `y2|wood_typering-porous`, `y3|wood_typering-porous`,
    `y1|wood_typediffuse-porous:climwinmean`, `y2|wood_typediffuse-porous:climwinmean`, `y3|wood_typediffuse-porous:climwinmean`,
    `y1|wood_typering-porous:climwinmean`, `y2|wood_typering-porous:climwinmean`, `y3|wood_typering-porous:climwinmean`,
    # Random effects
    b[term,group]
  ) %>%
  select(-.chain, -.iteration) %>%
  arrange(.draw)





# Analyze fixed effects ---------------------------------
# Extract draws of fixed effects:
fixed_effects <- posterior_draws %>%
  filter(.variable != "b") %>%
  ungroup() %>%
  select(-c(term, group)) %>%
  # Split DOY
  separate(.variable, c("perc", "coefficient"), sep = "\\|") %>%
  mutate(
    perc = case_when(
      perc == "y1" ~ "DOY_25",
      perc == "y2" ~ "DOY_50",
      perc == "y3" ~ "DOY_75"
    )
  ) %>%
  pivot_wider(names_from = coefficient, values_from = .value)

# Compute posterior means
posterior_means_fixed_effects <- fixed_effects %>%
  group_by(perc) %>%
  summarize(
    `(Intercept)` = mean(`(Intercept)`),
    `wood_typering-porous` = mean(`wood_typering-porous`),
    `wood_typediffuse-porous:climwinmean` = mean(`wood_typediffuse-porous:climwinmean`),
    `wood_typering-porous:climwinmean` = mean(`wood_typering-porous:climwinmean`)
    )

# Compare to regression table. Identical! Yay!
posterior_means_fixed_effects
bayesian_regression_table %>%
  select(coefficient, mean)




# Analyze random effects ---------------------------------
# Skipped




# Fig6: Plot of regression of DOY over climwinmeans with credible intervals ----
# Extract predicted DOY_25, DOY_50, DOY_75
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

predictions %>%
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


fig6 <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey") +
  stat_lineribbon(data = predictions, aes(x = climwinmean, y = predictions_rstanarm, group = perc, col = perc), .width = c(.99, .95),  color = "#08519C") +
  geom_point(data = Wood_pheno_table, aes(x = climwinmean, y = DOY, col = perc)) +
  # geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  facet_grid(perc~wood_type) +
  labs(x = "Climwin mean temperature (relative to 65°F)", y = "DOY", col = "Percentile", main = "Relationship of DOY versus climwin mean temperature") +
  geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
fig6
ggsave(filename = "doc/manuscript/tables_figures/fig6.png", width = 14.7*.7, height = 10.9*.7, plot = fig6)

# Sanity check this plot with regression table intercepts and slopes
posterior_means_fixed_effects
















