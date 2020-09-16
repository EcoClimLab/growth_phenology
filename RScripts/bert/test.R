# 2020/9/15 -------
# Testing plotting credible intervals from http://mjskay.github.io/tidybayes/
library(rstanarm)
library(tidybayes)
library(plotly)
library(broom)
library(modelr)





# Visualize relationship between DOY and marchmeans -----------------------
#
# Run wood_phenology_bert.R lines 1-96 first
#
set.seed(76)
random_tags <- Wood_pheno_table$tag %>%
  unique() %>%
  sample(size = 20)

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





# Fit SIMPLE multivariate model using marchmeans -----------------------
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
  iter = 5000
)
save(joint_model_marchmeans, file = "results/2020-09-16_preliminary_credible_intervals_rstanarm_model.RData")

# Hack bayesian regression table:
joint_model_marchmeans %>%
  summary() %>%
  as_tibble(rownames = "coefficient") %>%
  select(coefficient, mean, `2.5%`, `97.5%`) %>%
  filter(!str_detect(coefficient, "Sigma")) %>%
  filter(str_detect(coefficient, "(Intercept)") | str_detect(coefficient, "marchmean"))

# Compare with standard lm output
lm(DOY ~ perc + marchmean:perc, data = temp_data) %>%
  tidy()





# Work with posterior draws -----------------------
# Reference: http://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html
# Used to identify fixed effects of interest:
joint_model_marchmeans %>%
  get_variables() %>%
  dput()

# Extract draws from posterior distributions
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



# Extract draws corresponding to fixed effects
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

# Posterior means of all fixed effects parameters. Compare to hack regression table above
posterior_lines <- fixed_effects %>%
  group_by(perc) %>%
  summarize(`(Intercept)` = mean(`(Intercept)`), marchmean = mean(marchmean))



# Extract draws corresponding to random effects
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

# For each covariable combination, extract fitted value
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


# File reprex on this: mean_DOY_hat is not working right for stanmvreg objects
predictions %>%
  group_by(perc) %>%
  summarize(mean_DOY = mean(DOY), mean_DOY_hat = mean(.prediction), mean_DOY_hat_new = mean(.predictions_new))




# Hallelujah! -------
ggplot() +
  stat_lineribbon(data = predictions, aes(x = marchmean, y = .predictions_new, group = perc, col = perc), .width = c(.99, .95),  color = "#08519C") +
  geom_point(data = temp_data, aes(x = marchmean, y = DOY, col = perc)) +
  geom_abline(data = posterior_lines, aes(intercept = `(Intercept)`, slope = marchmean, col = perc), size = 1) +
  scale_fill_brewer() +
  facet_wrap(~perc) +
  labs(x = "March mean temperature (recentered at mean)", y = "DOY", col = "Percentile", main = "Posterior credible intervals")
ggsave(filename = "results/2020-09-16_preliminary_credible_intervals.png")










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
