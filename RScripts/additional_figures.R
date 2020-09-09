library(tidyverse)

# Figure numbers from: https://github.com/SCBI-ForestGEO/growth_phenology/issues/13



# Figure 1: Logistic growth curve and parameter illustration -------------------
# Set parameters from McMahon & Parker (2014)
# 1. L: lower asymptotes
# 2. K: upper asymptotes
# 3. doy.ip: inflection point
# 4. r: slope of curve at inflection point
# 5. theta: theta = 1 indicates pre/post inflection post symmetry
K <- 13.2
L <- 13.8
doy.ip <- 200
r <- 0.075
theta <- 2
params <- c(K, L, doy.ip, r, theta)

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
    growth = c(0.25, 0.5, 0.75, 1)
  )

# Function to create polygons to ggplot
create_growth_polygon <- function(percent){
  index <- case_when(
    percent == 0.25 ~ 1,
    percent == 0.5 ~ 2,
    percent == 0.75 ~ 3)

  growth_doy_domain <- seq(from = doy_diameter_quartile$doy[index], to = doy_diameter_quartile$doy[4])
  n_days_domain <- length(growth_doy_domain)

  growth_polygon <- tibble(
    doy = c(growth_doy_domain[index], growth_doy_domain, growth_doy_domain[n_days_domain], growth_doy_domain[index]),
    diameter = c(K, lg5.pred(params, growth_doy_domain), K, K)
  )

  return(growth_polygon)
}


# Output figure
ggplot() +
  # Mark 25%, 50%, 75%, 100% DOY
  geom_vline(data = doy_diameter_quartile, mapping = aes(xintercept = doy), linetype = "dashed", col = "grey") +
  geom_polygon(data = create_growth_polygon(percent = 0.25), mapping = aes(x = doy, y = diameter), fill = grey(level = 0.8)) +
  geom_polygon(data = create_growth_polygon(percent = 0.50), mapping = aes(x = doy, y = diameter), fill = grey(level = 0.6)) +
  geom_polygon(data = create_growth_polygon(percent = 0.75), mapping = aes(x = doy, y = diameter), fill = grey(level = 0.4)) +
  # Plot L & K asymptotes
  geom_hline(yintercept = params[1:2], linetype = "dashed") +
  # Plot true growth curve
  geom_line(data = true_values, mapping = aes(x = doy, y = diameter)) +
  # Plot observed values
  geom_point(data = observed_values, mapping = aes(x = doy, y = diameter), shape = 21, colour = "black", fill = "white", size = 3, stroke = 1) +
  # Plot
  labs(x = "Day of year", y = "Diameter (cm)")
# TODO: Add
# - Explanations of parameters
# - 25-75 DOY window
# - max growth rate
# - total annual growth

fig_width <- 8
ggsave("doc/manuscript/tables_figures/fig1.png", width = fig_width, height = fig_width / 1.52)



# Figure 2:

