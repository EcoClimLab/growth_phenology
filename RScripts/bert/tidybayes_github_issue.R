# Filed here: https://github.com/mjskay/tidybayes/issues/271

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
