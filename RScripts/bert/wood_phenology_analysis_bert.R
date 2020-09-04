# Load packages and data ---------------------------------------
library(tidyverse)
library(lubridate)
library(rstanarm)
library(tidybayes)
library(patchwork)




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
  )
View(Wood_pheno_table)




# First model: Fit multivariate model using marchmeans -----------------------
joint_model_marchmeans <- stan_mvmer(
  formula = list(
    DOY_25 ~ wood_type + wood_type:marchmean + (1|sp) + (1|tag),
    DOY_50 ~ wood_type + wood_type:marchmean + (1|sp) + (1|tag),
    DOY_75 ~ wood_type + wood_type:marchmean + (1|sp) + (1|tag)
  ),
  # Note we transform the data from tall/tidy format to wide format first:
  data = Wood_pheno_table %>% pivot_wider(names_from = perc, values_from = DOY),
  seed = 349,
  # Once we feel good about our results, increase these values. The code will
  # take longer to run however
  chains = 2,
  iter = 1000
)

# Used to identify fixed effects of interest:
joint_model_marchmeans %>%
  get_variables() %>%
  dput()

# Extract draws from posterior distributions
# See http://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html
posterior_draws_marchmeans <- joint_model_marchmeans %>%
  gather_draws(
    # This is a hack:
    `y1|(Intercept)`, `y2|(Intercept)`, `y3|(Intercept)`, `y1|wood_typering-porous`,
    `y1|wood_typediffuse-porous:marchmean`, `y1|wood_typering-porous:marchmean`,
    `y2|wood_typering-porous`, `y2|wood_typediffuse-porous:marchmean`,
    `y2|wood_typering-porous:marchmean`, `y3|wood_typering-porous`,
    `y3|wood_typediffuse-porous:marchmean`, `y3|wood_typering-porous:marchmean`
    )

# Posterior means of all parameters. Note we need to correct the baseline (DP)
# vs offset (RP) interpretation of intercepts. Slopes for marchmean are fine
posterior_means_marchmeans <- posterior_draws_marchmeans %>%
  group_by(.variable) %>%
  summarize(value = mean(.value))
View(posterior_means_marchmeans)

# This is a manually done hack and hence is really brittle. Need to change this
# later to be more robust
line_info_marchmeans <- posterior_means_marchmeans %>%
  mutate(
    wood_type = c("diffuse-porous", "diffuse-porous", "ring-porous", "ring-porous") %>% rep(times = 3),
    perc = c("DOY_25", "DOY_50", "DOY_75") %>% rep(each = 4),
    parameter = c("intercept", "slope", "intercept", "slope") %>% rep(times = 3)
    ) %>%
  select(-.variable) %>%
  pivot_wider(names_from = parameter, values_from = value) %>%
  mutate(
    intercept = ifelse(wood_type == "ring-porous" & perc == "DOY_25", 168 + 20.5, intercept),
    intercept = ifelse(wood_type == "ring-porous" & perc == "DOY_50", 177 + 15.3, intercept),
    intercept = ifelse(wood_type == "ring-porous" & perc == "DOY_75", 187 + 3.76, intercept)
  )
View(line_info_marchmeans)

# Plot
plot_marchmeans <- ggplot() +
  facet_wrap(~wood_type) +
  geom_point(data = Wood_pheno_table, aes(x = marchmean, y = DOY, col = perc)) +
  labs(x = "Mean march daily maximum temperature", y = "DOY") +
  geom_abline(data = line_info_marchmeans, aes(intercept = intercept, slope = slope, col = perc), size = 1)
plot_marchmeans




# Second model: Fit multivariate model using climwinmeans -----------------------
joint_model_climwinmeans <- stan_mvmer(
  formula = list(
    DOY_25 ~ wood_type + wood_type:climwinmean + (1|sp) + (1|tag),
    DOY_50 ~ wood_type + wood_type:climwinmean + (1|sp) + (1|tag),
    DOY_75 ~ wood_type + wood_type:climwinmean + (1|sp) + (1|tag)
  ),
  # Note we transform the data from tall/tidy format to wide format first:
  data = Wood_pheno_table %>% pivot_wider(names_from = perc, values_from = DOY),
  seed = 349,
  # Once we feel good about our results, increase these values. The code will
  # take longer to run however
  chains = 2,
  iter = 1000
)

# Used to identify fixed effects of interest:
joint_model_climwinmeans %>%
  get_variables() %>%
  dput()

# Extract draws from posterior distributions
# See http://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html
posterior_draws_climwinmeans <- joint_model_climwinmeans %>%
  gather_draws(
    # This is a hack:
    `y1|(Intercept)`, `y2|(Intercept)`, `y3|(Intercept)`, `y1|wood_typering-porous`,
    `y1|wood_typediffuse-porous:climwinmean`, `y1|wood_typering-porous:climwinmean`,
    `y2|wood_typering-porous`, `y2|wood_typediffuse-porous:climwinmean`,
    `y2|wood_typering-porous:climwinmean`, `y3|wood_typering-porous`,
    `y3|wood_typediffuse-porous:climwinmean`, `y3|wood_typering-porous:climwinmean`
  )

# Posterior means of all parameters. Note we need to correct the baseline (DP)
# vs offset (RP) interpretation of intercepts. Slopes for climwinmean are fine
posterior_means_climwinmeans <- posterior_draws_climwinmeans %>%
  group_by(.variable) %>%
  summarize(value = mean(.value))
View(posterior_means_climwinmeans)


# This is a manually done hack and hence is really brittle. Need to change this
# later to be more robust
line_info_climwinmeans <- posterior_means_climwinmeans %>%
  mutate(
    wood_type = c("diffuse-porous", "diffuse-porous", "ring-porous", "ring-porous") %>% rep(times = 3),
    perc = c("DOY_25", "DOY_50", "DOY_75") %>% rep(each = 4),
    parameter = c("intercept", "slope", "intercept", "slope") %>% rep(times = 3)
  ) %>%
  select(-.variable) %>%
  pivot_wider(names_from = parameter, values_from = value) %>%
  mutate(
    intercept = ifelse(wood_type == "ring-porous" & perc == "DOY_25", 260 - 5.16, intercept),
    intercept = ifelse(wood_type == "ring-porous" & perc == "DOY_50", 298 - 46.5, intercept),
    intercept = ifelse(wood_type == "ring-porous" & perc == "DOY_75", 318 - 78, intercept)
  )
View(line_info_climwinmeans)

# Plot
plot_climwinmeans <- ggplot() +
  facet_wrap(~wood_type) +
  geom_point(data = Wood_pheno_table, aes(x = climwinmean, y = DOY, col = perc)) +
  labs(x = "Mean climwin daily maximum temperature", y = "DOY") +
  geom_abline(data = line_info_climwinmeans, aes(intercept = intercept, slope = slope, col = perc), size = 1) +
  # Add labels for climwin windows:
  geom_text(data = climwin_windows, aes(label = window), x = 57, y = 10, hjust = 0)
plot_climwinmeans






# Compare models ---------------
# This uses the patchwork pkg:
plot_marchmeans / plot_climwinmeans
ggsave(filename = "results/2020-09-04_marchmeans-vs-climwinmeans.png")


