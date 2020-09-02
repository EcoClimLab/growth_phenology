# Load packages and data ---------------------------------------
library(tidyverse)
library(lubridate)
library(rstanarm)
library(ggrepel)
library(tidybayes)

# Get avg tmax for march every year:
marchmeans <-
  read_csv("climate data/NCDC_NOAA_precip_temp.csv") %>%
  mutate(
    DATE = dmy(DATE),
    months = month(DATE, label = TRUE, abbr = FALSE)
  ) %>%
  filter(
    # Missing values
    !is.na(flag) & !is.na(flagdp) & !is.na(TMAX),
    months == "March"
  ) %>%
  group_by(year) %>%
  summarize(marchmean = mean(TMAX))

# Get all growth data
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V4.csv") %>%
  left_join(marchmeans, by = "year") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))


# Fit multivariate model ---------------------------------------
DOY_all_tags <- Wood_pheno_table %>%
  select(-c(tot, dbh, max_rate_DOY, max_rate)) %>%
  mutate(
    perc = case_when(
      perc == 0.25 ~ "DOY_25",
      perc == 0.5 ~ "DOY_50",
      perc == 0.75 ~ "DOY_75"
    )
  )

joint_model <- stan_mvmer(
  formula = list(
    DOY_25 ~ wood_type + wood_type:marchmean + (1|sp) + (1|tag),
    DOY_50 ~ wood_type + wood_type:marchmean + (1|sp) + (1|tag),
    DOY_75 ~ wood_type + wood_type:marchmean + (1|sp) + (1|tag)
  ),
  # Note we transform the data first
  data = DOY_all_tags %>% pivot_wider(names_from = perc, values_from = DOY),
  chains = 2,
  seed = 349,
  iter = 1000
)


# Extract posterior samples ---------------------------------------
joint_model %>%
  get_variables() %>%
  dput()

# This is a hack
posterior_draws <- joint_model %>%
  gather_draws(
    `y1|(Intercept)`, `y2|(Intercept)`, `y3|(Intercept)`, `y1|wood_typering-porous`,
    `y1|wood_typediffuse-porous:marchmean`, `y1|wood_typering-porous:marchmean`,
    `y2|wood_typering-porous`, `y2|wood_typediffuse-porous:marchmean`,
    `y2|wood_typering-porous:marchmean`, `y3|wood_typering-porous`,
    `y3|wood_typediffuse-porous:marchmean`, `y3|wood_typering-porous:marchmean`
    )

posterior_means <- posterior_draws %>%
  group_by(.variable) %>%
  summarize(value = mean(.value))
posterior_draws %>%
  ggplot(aes(x = .variable, y = .value)) +
  geom_boxplot() +
  coord_flip()


# This is a hack
line_info <- posterior_means %>%
  mutate(
    wood_type = c("diffuse-porous", "diffuse-porous", "ring-porous", "ring-porous") %>% rep(times = 3),
    perc = c("DOY_25", "DOY_50", "DOY_75") %>% rep(each = 4),
    parameter = c("intercept", "slope", "intercept", "slope") %>% rep(times = 3)
    ) %>%
  select(-.variable) %>%
  pivot_wider(names_from = parameter, values_from = value) %>%
  mutate(
    intercept = ifelse(wood_type == "ring-porous" & perc == "DOY_25", 165 + 5.55, intercept),
    intercept = ifelse(wood_type == "ring-porous" & perc == "DOY_50", 177 + 12.9, intercept),
    intercept = ifelse(wood_type == "ring-porous" & perc == "DOY_75", 187 + 19, intercept)
  )

ggplot() +
  facet_wrap(~wood_type) +
  geom_point(data = DOY_all_tags, aes(x = marchmean, y = DOY, col = perc)) +
  labs(x = "Mean march daily maximum temperature", y = "DOY") +
  geom_abline(data = line_info, aes(intercept = intercept, slope = slope, col = perc), size = 1)


