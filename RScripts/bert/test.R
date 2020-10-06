# 2020/9/16, 2020/10/5, 2020/10/6 -------
# Testing plotting credible intervals from http://mjskay.github.io/tidybayes/
library(rstanarm)
library(tidybayes)
library(plotly)
library(broom)
library(modelr)





# Visualize relationship between DOY and marchmeans ----------------------------
#
# Run wood_phenology_analysis_bert.R lines 1-96 first
#
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













# 2020/9/11 -------
# Testing plotting credible intervals from http://mjskay.github.io/tidybayes/
library(rstanarm)
library(tidybayes)
library(broom)
library(brms)
library(moderndive)
library(plotly)
library(modelr)



# First model: Fit multivariate model using marchmeans -----------------------
set.seed(76)
random_tags <- Wood_pheno_table$tag %>% unique() %>% sample(size = 20)

temp_data <- Wood_pheno_table %>%
  mutate(marchmean = marchmean - mean(marchmean)) %>%
  filter(tag %in% random_tags) %>%
  select(tag, year, perc, DOY, marchmean) %>%
  arrange(tag, year, perc)
temp_data_wide <- temp_data %>%
  pivot_wider(names_from = perc, values_from = DOY)

ggplot(temp_data, aes(x = marchmean, y = DOY, col = perc)) +
  geom_point() +
  facet_wrap(~perc) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed")
ggplotly()


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
  iter = 1000
)

# Hack regression table
joint_model_marchmeans %>%
  summary() %>%
  as_tibble(rownames = "coefficient") %>%
  select(coefficient, mean, `2.5%`, `97.5%`) %>%
  filter(!str_detect(coefficient, "Sigma")) %>%
  filter(str_detect(coefficient, "(Intercept)") | str_detect(coefficient, "marchmean"))

lm(DOY ~ perc + marchmean:perc, data = temp_data) %>%
  tidy()


# Used to identify fixed effects of interest:
joint_model_marchmeans %>%
  get_variables() %>%
  dput()

# Extract draws from posterior distributions
# See http://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html
# 6000 rows = 1000 simulations x
posterior_draws_marchmeans <- joint_model_marchmeans %>%
  gather_draws(
    # This is a hack:
    `y1|(Intercept)`, `y2|(Intercept)`, `y3|(Intercept)`,
    `y1|marchmean`, `y2|marchmean`, `y3|marchmean`
    # `y1|wood_typering-porous`, `y2|wood_typering-porous`, `y3|wood_typering-porous`
  )


# Posterior means of all parameters. Compare to hack regression table above
posterior_means_marchmeans <- posterior_draws_marchmeans %>%
  group_by(.variable) %>%
  summarize(.value = mean(.value))
posterior_means_marchmeans


# Build desired plot
posterior_lines <- posterior_means_marchmeans %>%
  separate(.variable, c("perc", "coefficient"), sep = "\\|") %>%
  mutate(
    perc = case_when(
      perc == "y1" ~ "DOY_25",
      perc == "y2" ~ "DOY_50",
      perc == "y3" ~ "DOY_75"
    )
  ) %>%
  pivot_wider(names_from = coefficient, values_from = .value)

ggplot() +
  stat_lineribbon(data = values, aes(x = marchmean, y = .prediction, group = perc, col = perc), .width = c(.99, .95),  color = "#08519C") +
  geom_point(data = temp_data, aes(x = marchmean, y = DOY, col = perc)) +
  geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  facet_wrap(~perc) +
  scale_fill_brewer()




# This looks good
joint_model_marchmeans %>%
  get_variables() %>%
  dput()


posterior_draws <- joint_model_marchmeans %>%
  gather_draws(
    # This is a hack:
    `y1|(Intercept)`, `y2|(Intercept)`, `y3|(Intercept)`,
    `y1|marchmean`, `y2|marchmean`, `y3|marchmean`,
    b[term,group]
  ) %>%
  select(-.chain, -.iteration) %>%
  arrange(.draw)

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


predictions <- temp_data %>%
  add_predicted_draws(joint_model_marchmeans) %>%
  mutate(.predictions_new = 0)


for(i in 1:nrow(temp_data)){

  temp_data_i <- temp_data %>%
    slice(i)

  fixed_effects_i <- fixed_effects %>%
    filter(perc == temp_data_i$perc)

  random_effects_i <- random_effects %>%
    filter(tag == temp_data_i$tag, perc == temp_data_i$perc)


  DOY_hat <- random_effects_i$.value +
    fixed_effects_i$`(Intercept)` +
    fixed_effects_i$marchmean * temp_data_i$marchmean

  index <- predictions$tag == temp_data_i$tag &
    predictions$year == temp_data_i$year &
    predictions$perc == temp_data_i$perc
  predictions[index,]$.predictions_new <- DOY_hat
}


# File reprex on this
predictions %>%
  group_by(perc) %>%
  summarize(mean_DOY = mean(DOY), mean_DOY_hat = mean(.prediction), mean_DOY_hat_new = mean(.predictions_new))


ggplot() +
  stat_lineribbon(data = predictions, aes(x = marchmean, y = .predictions_new, group = perc, col = perc), .width = c(.99, .95),  color = "#08519C") +
  geom_point(data = temp_data, aes(x = marchmean, y = DOY, col = perc)) +
  geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  facet_wrap(~perc) +
  scale_fill_brewer()
