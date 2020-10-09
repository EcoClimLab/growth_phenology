# 2020/10/9 -------
# Relationship between monthly average TMAX and total precipitation
temp_and_precip_data <- read_csv("climate data/NCDC_NOAA_precip_temp.csv") %>%
  mutate(
    DATE = dmy(DATE),
    month = floor_date(DATE, unit = "month"),
    months = month(DATE, label = TRUE, abbr = FALSE),
    season = case_when(
      months %in% c("March", "April", "May") ~ "spring",
      months %in% c("June", "July", "August") ~ "summer",
      months %in% c("September", "October", "November") ~ "fall",
      months %in% c("December", "January", "February") ~ "winter"
    ),
    season = factor(season, levels = c("spring", "summer", "fall", "winter"))
  ) %>%
  filter(!is.na(PRCP) & !is.na(TMAX)) %>%
  group_by(month, months, season) %>%
  summarize(total_precip = sum(PRCP), mean_tmax = mean(TMAX))

# Base plot
base_plot <- ggplot(temp_and_precip_data, aes(x = mean_tmax, y = total_precip)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean daily TMAX per month", y = "Total precipitation per month")

# Split by month
by_month <- base_plot +
  facet_wrap(~months, nrow = 4) +
  labs(title = "Relationship of temp and precipitation by month")
by_month
ggsave(filename = "results/2020-10-09_temp_vs_precip_by_month.png", plot = by_month)

# Split by season
by_season <- base_plot +
  facet_wrap(~season, nrow = 2) +
  labs(title = "Relationship of temp and precipitation by season",
       subtitle = "Spring = Mar + Apr + May, Summer = Jun + Jul + Aug, ...")
by_season
ggsave(filename = "results/2020-10-09_temp_vs_precip_by_season.png", plot = by_season)




# 2020/9/16, 2020/10/5, 2020/10/6 -------
# Testing plotting credible intervals from http://mjskay.github.io/tidybayes/
library(tidyverse)
library(lubridate)
library(rstanarm)
library(tidybayes)
library(broom)
library(modelr)
library(patchwork)


# Visualize relationship between DOY and marchmeans ----------------------------
# Get growth data
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V4.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))

# Create temperature variable
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
  )





# Visualize relationship between DOY and marchmeans ----------------------------
set.seed(76)
random_tags <- Wood_pheno_table %>%
  pull(tag) %>%
  unique() %>%
  sample(10)

temp_data <- Wood_pheno_table %>%
  mutate(marchmean = marchmean - mean(marchmean)) %>%
  # Random sample to speed up computation:
  filter(tag %in% random_tags) %>%
  select(tag, year, perc, DOY, marchmean) %>%
  arrange(tag, year, perc)

temp_data_wide <- temp_data %>%
  pivot_wider(names_from = perc, values_from = DOY)

ggplot(temp_data, aes(x = marchmean, y = DOY, col = perc)) +
  geom_point() +
  facet_wrap(~perc) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "All trees", x = "Mean-recentered march mean temperature")





# Fit multivariate model -------------------------------------------------------
joint_model_marchmeans <- stan_mvmer(
  formula = list(
    # DOY_25 ~ marchmean + (1|sp) + (1|tag),
    # DOY_50 ~ marchmean + (1|sp) + (1|tag),
    # DOY_75 ~ marchmean + (1|sp) + (1|tag),
    DOY_25 ~ marchmean + (1|tag),
    DOY_50 ~ marchmean + (1|tag),
    DOY_75 ~ marchmean + (1|tag)
  ),
  # Note we transform the data from tall/tidy format to wide format first:
  data = temp_data_wide,
  seed = 349,
  # Once we feel good about our results, increase these values. The code will
  # take longer to run however
  chains = 2,
  iter = 2000
)
# save(joint_model_marchmeans, file = "results/2020-09-16_preliminary_credible_intervals_rstanarm_model.RData")

# Hack bayesian regression table:
bayesian_regression_table <- joint_model_marchmeans %>%
  summary() %>%
  as_tibble(rownames = "coefficient") %>%
  select(coefficient, mean, `2.5%`, `97.5%`) %>%
  filter(!str_detect(coefficient, "Sigma")) %>%
  filter(str_detect(coefficient, "(Intercept)") | str_detect(coefficient, "marchmean"))
bayesian_regression_table

# Compare with standard lm output
frequentist_regression_table <- lm(DOY ~ perc + marchmean:perc, data = temp_data) %>%
  tidy()
frequentist_regression_table





# Posterior draws --------------------------------------------------------------
# Reference: http://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html
# Used to identify fixed effects of interest:
joint_model_marchmeans %>%
  get_variables() %>%
  dput()

# Extract posterior draws
posterior_draws <- joint_model_marchmeans %>%
  gather_draws(
    # This is a hack:
    `y1|(Intercept)`, `y2|(Intercept)`, `y3|(Intercept)`,
    `y1|marchmean`, `y2|marchmean`, `y3|marchmean`,
    # `y1|wood_typering-porous`, `y2|wood_typering-porous`, `y3|wood_typering-porous`
    b[term,group]
  ) %>%
  select(-.chain, -.iteration) %>%
  arrange(.draw)





# Fixed effects posterior draws ------------------------------------------------
# Executive summary: Posterior regression parameters look good
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
fixed_effects

# Posterior means of all fixed effects parameters
posterior_lines <- fixed_effects %>%
  group_by(perc) %>%
  summarize(`(Intercept)` = mean(`(Intercept)`), marchmean = mean(marchmean))
posterior_lines

# Identical to Bayesian regression table! Yay!
bayesian_regression_table





# Extract draws corresponding to random effects --------------------------------
# Executive summary: Looks good, I think
random_effects <- posterior_draws %>%
  filter(.variable == "b") %>%
  ungroup() %>%
  # Split DOY
  separate(term, c("perc", "coefficient"), sep = "\\|") %>%
  mutate(
    perc = case_when(
      perc == "y1" ~ "DOY_25",
      perc == "y2" ~ "DOY_50",
      perc == "y3" ~ "DOY_75"
    )
  ) %>%
  mutate(tag = str_replace_all(group, "tag:", "")) %>%
  select(-.variable, -group)
random_effects





# Extract predicted values using tidybayes::add_predicted_draws() --------------
# ISSUE: Predicted values look off so I filed issue https://github.com/mjskay/tidybayes/issues/271
# See RScripts/bert/tidybayes_github_issue.R
predictions <- temp_data %>%
  add_predicted_draws(joint_model_marchmeans)

predictions %>%
  group_by(perc) %>%
  summarize(mean_observed = mean(DOY), mean_predicted = mean(.prediction))

# Manually compute predicted values instead of using add_predicted_draws()
predictions <- predictions %>%
  mutate(predictions_manual = 0)

for(i in 1:nrow(temp_data)){
  temp_data_i <- temp_data %>%
    slice(i)
  tag_i <- temp_data_i$tag
  year_i <- temp_data_i$year
  perc_i <- temp_data_i$perc
  marchmean_i <- temp_data_i$marchmean

  # Get all fixed effects draws
  fixed_effects_i <- fixed_effects %>%
    filter(perc == perc_i)

  # Get all random effects draws
  random_effects_i <- random_effects %>%
    filter(perc == perc_i, tag == tag_i)

  # Compute predicted values
  index_i <- predictions$tag == tag_i &
    predictions$year == year_i &
    predictions$perc == perc_i

  predictions[index_i, "predictions_manual"] <- random_effects_i$.value +
    fixed_effects_i$`(Intercept)` +
    fixed_effects_i$marchmean * marchmean_i

  round(i/nrow(temp_data),3) %>% print()
}

# mean_DOY_hat is not working right for stanmvreg objects
predictions %>%
  ungroup() %>%
  group_by(perc) %>%
  summarize(mean_observed = mean(DOY), mean_predicted = mean(.prediction), mean_predicted_manual = mean(predictions_manual))



# Use rstanarm::posterior_predict() instead of using add_predicted_draws()
y_hat_25 <- joint_model_marchmeans %>%
  posterior_predict(m = 1)

# Rows are posterior draws = # of chains x (iter x 0.5 for burn-in)
dim(y_hat_25)
y_hat_25[1:5, 1:5]

# Watch out how you convert to vector. For a given data point in the input data,
# we want all simulations
y_hat_25 %>% c() %>% .[1:5]

y_hat <- c(
  joint_model_marchmeans %>% posterior_predict(m = 1) %>% c(),
  joint_model_marchmeans %>% posterior_predict(m = 2) %>% c(),
  joint_model_marchmeans %>% posterior_predict(m = 3) %>% c()
)

predictions <- predictions %>%
  ungroup() %>%
  # Critical: sort by y outcome variable first
  arrange(perc, tag, year) %>%
  mutate(predictions_rstanarm = y_hat)

predictions %>%
  group_by(perc) %>%
  summarize(
    mean_observed = mean(DOY),
    mean_predicted = mean(.prediction),
    mean_predicted_manual = mean(predictions_manual),
    mean_predicted_rstanarm = mean(predictions_rstanarm)
  )

# Hallelujah! -------
ggplot() +
  stat_lineribbon(data = predictions, aes(x = marchmean, y = predictions_rstanarm, group = perc, col = perc), .width = c(.99, .95),  color = "#08519C") +
  # stat_lineribbon(data = predictions, aes(x = marchmean, y = predictions_manual, group = perc, col = perc), .width = c(.99, .95),  color = "#08519C") +
  geom_point(data = temp_data, aes(x = marchmean, y = DOY, col = perc)) +
  geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  facet_wrap(~perc) +
  labs(x = "March mean temperature (recentered at mean)", y = "DOY", col = "Percentile", main = "Posterior credible intervals")
# ggsave(filename = "results/2020-09-16_preliminary_credible_intervals.png", width = 14.7*.8, height = 10.9*.8)









# 2020/10/5 --------------------------------------------------------------------
# tidybayes GitHub issue filed here: https://github.com/mjskay/tidybayes/issues/271

library(tidyverse)
library(rstanarm)
library(tidybayes)


# Create 3-dim outcome multivariate example data set ----
multivariate_data <- bind_rows(
  tibble(x = rep(1:50, times = 4)) %>% mutate(obs = "y1", y = 0 + 0.4*x),
  tibble(x = rep(1:50, times = 4)) %>% mutate(obs = "y2", y = 30 + 0.4*x),
  tibble(x = rep(1:50, times = 4)) %>% mutate(obs = "y3", y = 60 + 0.4*x)
) %>%
  mutate(
    group = rep(1:4, each = 50) %>% rep(times = 3),
    y = y + rnorm(n())
  )

ggplot(multivariate_data, aes(x = x, y = y, col = obs)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Group means:
multivariate_data %>%
  group_by(obs) %>%
  summarize(mean_y = mean(y))


# Fit stan_mvmer() model ----
# Convert data to wide format
multivariate_data_wide <- multivariate_data %>%
  pivot_wider(names_from = obs, values_from = y)

stanmvreg_model <- stan_mvmer(
  formula = list(
    y1 ~ x + (1|group),
    y2 ~ x + (1|group),
    y3 ~ x + (1|group)
  ),
  data = multivariate_data_wide,
  seed = 76,
  chains = 1,
  iter = 2000
)


# Use tidybayes::add_predicted_draws to get posterior y-hats ----
multivariate_data %>%
  add_predicted_draws(stanmvreg_model) %>%
  group_by(obs) %>%
  summarize(mean_y = mean(y), mean_y_hat = mean(.prediction))


# Back to rstanarm::posterior_predict ----
stanmvreg_model %>%
  posterior_predict() %>%
  apply(1, mean) %>%
  mean()

# Looking at help file ?rstanarm::posterior_predict -> Usage ->
# extra argument "m" needed for models of class stanmvreg
# Defaults to m = 1

# If you specify m, you get correct posterior means
stanmvreg_model %>%
  posterior_predict(m = 1) %>%
  apply(1, mean) %>%
  mean()
stanmvreg_model %>%
  posterior_predict(m = 2) %>%
  apply(1, mean) %>%
  mean()
stanmvreg_model %>%
  posterior_predict(m = 3) %>%
  apply(1, mean) %>%
  mean()

multivariate_data %>%
  add_predicted_draws(stanmvreg_model, m = 2)
