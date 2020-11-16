library(tidyverse)
library(scales)
library(ggrepel)
library(tidybayes)

# Figure numbers from: https://github.com/SCBI-ForestGEO/growth_phenology/issues/13

# Figure 1: Logistic growth curve and parameter illustration -------------------
# Set parameters from McMahon & Parker (2014)
# 1. L: lower asymptotes
# 2. K: upper asymptotes
# 3. doy.ip: inflection point
# 4. r: value that determines slope of curve at inflection point ((K-L)*r/theta )/(1 + 1/theta)^2
# 5. theta: theta = 1 indicates pre/post inflection post symmetry
L <- 13
K <- 14
doy.ip <- 182
r <- 0.025
theta <- 1.5
params <- c(L, K, doy.ip, r, theta)
total_growth <- K - L

start_doy <- 1
end_doy <- 365

window_open <- 60
window_close <- 105

critical_temp_window <- tibble(
  x = c(window_open, window_close, window_close, window_open, window_open),
  y = c()
)

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

# 1. True growth (logistic curve):
true_values <- tibble(
  doy = seq(from = start_doy, to = end_doy, by = 1),
  diameter = lg5.pred(params, doy)
) %>%
  mutate(diff = diameter - lag(diameter))


# 2. Max growth rate/inflection point (tangent line):
# Incorrect intercept since r is not true slope at inflection point:
# intercept <- lg5.pred(params, doy.ip) - r *doy.ip

# Correct slope and intercept
r_true <- ((K-L)*r/theta )/(1 + 1/theta)^2
intercept_true <- lg5.pred(params, doy.ip) - r_true *doy.ip


# 3. Observed values (noise/error added) (points)
set.seed(76)
observed_values <- tibble(
  doy = seq(from = start_doy + 14, to = end_doy - 14, length = 24),
  diameter = lg5.pred(params, doy)
) %>%
  mutate(diameter = diameter + rnorm(n(), sd = 0.025))



# 4. Compute 25%/50%/75% growth values and DOY
doy_diameter_quartile <- bind_rows(
  tibble(doy = 1, diameter = L),
  true_values %>% filter(diameter >= (L + total_growth * .25)) %>% slice(1),
  true_values %>% filter(diameter >= (L + total_growth * .5)) %>% slice(1),
  true_values %>% filter(diameter >= (L + total_growth * .75)) %>% slice(1),
  tibble(doy = 365, diameter = K)
) %>%
  mutate(
    growth = c(0, 0.25, 0.5, 0.75, 1),
    label = c("0%", "25%", "50%", "75%", "100%"),
    label = factor(label, levels = c("0%", "25%", "50%", "75%", "100%"))
  )

# Function to create 25%/50%/75% growth polygons
create_growth_polygon <- function(doy_diameter_quartile, percent){
  index <- case_when(
    percent == 0.25 ~ 1,
    percent == 0.5 ~ 2,
    percent == 0.75 ~ 3
  )
  growth_doy_domain <- seq(from = doy_diameter_quartile$doy[index], to = doy_diameter_quartile$doy[4])
  n_days_domain <- length(growth_doy_domain)

  growth_polygon <- tibble(
    doy = c(growth_doy_domain[1], growth_doy_domain, growth_doy_domain[n_days_domain], growth_doy_domain[1]),
    diameter = c(L, lg5.pred(params, growth_doy_domain), L, L)
  )

  return(growth_polygon)
}


# Output figure
geom.text.size <- 4
theme.size <- (14/5) * geom.text.size


schematic <- ggplot() +
  # Overall theme:
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = theme.size),
    axis.title = element_text(size = theme.size)
  ) +
  labs(
    x = "Day of year (1 to 365)",
    y = "Diameter at breast height"
  ) +
  coord_cartesian(xlim = c(30, 280))+
  # Mark DOY's on x-axis:
  geom_vline(data = doy_diameter_quartile, aes(xintercept = doy), linetype = "dashed", show.legend = FALSE, col = "grey") +
  scale_x_continuous(
    breaks = c(doy_diameter_quartile$doy, doy.ip, window_open, window_close),
    labels = c(1, expression(DOY[25]), expression(DOY[50]), expression(DOY[75]), 365, expression(DOY[ip]), expression(w[open]), expression(w[close]))
  ) +
  # Mark growth percentages on y-axis:
  geom_hline(data = doy_diameter_quartile, aes(yintercept = diameter), linetype = "dashed", show.legend = FALSE, col = "grey") +
  scale_y_continuous(
    breaks = c(L, K),
    labels = c("L", "K"),
    sec.axis = sec_axis(
      ~ . * 1,
      breaks = doy_diameter_quartile$diameter ,
      labels = doy_diameter_quartile$label,
      name = expression(paste("% of (fitted) annual growth ", Delta[DBH],  " = K - L"))
    )
  ) +
  # Inflection point & max growth rate
  annotate("point", x = doy.ip, y = lg5.pred(params, doy.ip), shape = 18, size = 4) +
  geom_vline(xintercept = doy.ip, linetype = "dotted") +
  geom_abline(intercept = intercept_true, slope = r_true, linetype = "dotted") +
  annotate(
    "text",
    x = doy.ip,
    y = lg5.pred(params, doy.ip) + 0.025,
    label = expression(paste(g[max], " = slope of tangent line  ")),
    hjust = 1,
    size = geom.text.size
  ) +
  # Growth window:
  geom_segment(
    aes(
      x = doy_diameter_quartile$doy[2],
      y = L + 0.05,
      xend = doy_diameter_quartile$doy[4],
      yend = L + 0.05
    ),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both")
  ) +
  annotate(
    "text",
    x = doy_diameter_quartile$doy[2] + (doy_diameter_quartile$doy[4] - doy_diameter_quartile$doy[2]) * 0.5,
    y = L + 0.025,
    label = expression(L[PGS]),
    hjust = 0.5,
    size = geom.text.size
  ) +
  # Critical temperature window:
  geom_rect(aes(xmin = window_open, xmax = window_close, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  geom_segment(
    aes(
      x = window_open,
      y = L + (K-L) * 0.45 + 0.025,
      xend = window_close,
      yend = L + (K-L) * 0.45 + 0.025
    ),
    arrow = arrow(length = unit(0.25, "cm"), ends = "both")
  ) +
  annotate(
    "text",
    x = window_open + (window_close - window_open) * 0.5,
    y = L + (K-L) * 0.45,
    label = "Critical\nTemperature\nWindow",
    hjust = 0.5,
    vjust = 1,
    size = geom.text.size
  ) +
  # True growth curve
  geom_line(data = true_values, mapping = aes(x = doy, y = diameter)) +
  # Observed values
  geom_point(data = observed_values, mapping = aes(x = doy, y = diameter), shape = 21, colour = "black", fill = "white", size = 3, stroke = 1)

schematic
fig_width <- 9
ggsave("doc/manuscript/tables_figures/schematic.png", plot = schematic, width = fig_width, height = fig_width * 9 / 16)





#----









