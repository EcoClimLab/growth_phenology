# Load packages and data ---------------------------------------
library(tidyverse)
library(lubridate)
library(tidybayes)
library(patchwork)
library(knitr)
library(scales)
options(mc.cores = parallel::detectCores())
library(rstanarm)
library(broom.mixed)

# Get growth data ----------------------------------
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_HarvardForest_V2.csv") %>%
  # Keep only RP and DP for now
  filter(wood_type != "other") %>%
  #filter(tot >= 1) %>%
  #filter(tot <= 12.06)%>%
  # Rename ring porous to not have a space
  mutate(wood_type = ifelse(wood_type == "ring porous", "ring-porous", wood_type))
Wood_pheno_table$tag <- substr(Wood_pheno_table$tag,1, nchar(as.character(Wood_pheno_table$tag))-4)

twofive <- subset(Wood_pheno_table, perc == .25)
fifty <- subset(Wood_pheno_table, perc == .5)
sevenfive <- subset(Wood_pheno_table, perc == .75)
#25-50
twofifty <- cbind(twofive,fifty$DOY)
twofifty$twentyfive_to_fifty <- twofifty$`fifty$DOY`-twofifty$DOY
twofifty <- twofifty[,c(3,6,12)]
#50-75
fiftyseventy <- cbind(fifty, sevenfive$DOY)
fiftyseventy$fifty_to_seventy <- fiftyseventy$`sevenfive$DOY`-fiftyseventy$DOY
fiftyseventy <- fiftyseventy[,c(3,6,12)]
#25-75
twosevenfive <- cbind(twofive, sevenfive$DOY)
twosevenfive$seasonlength <- twosevenfive$`sevenfive$DOY`-twosevenfive$DOY
twosevenfive <- twosevenfive[,c(3,6,12)]


# Create temperature variables ----------------------------------
# 0. Get all weather data
weatherdata <-
  read_csv("climate data/HF_weatherdata.csv") %>%
  filter(!is.na(airtmax))

#mutate(
#  DATE = dmy(DATE),
#  months = month(DATE, label = TRUE, abbr = FALSE)
#) %>%
# Remove entries with no tmax data
#%>%
# Rename RP flag set by Cam
#rename(flagrp = flag)
climwindows <-
  read.csv("results/Climwin_results/Weekly/SCBI/weekly_climwin_results.csv") %>%
  filter(wood_type != "other") %>%
  mutate(
    median_windowopendate = strptime(median_windowopendate, format = "%m/%d/%y"),
    median_windowclosedate = strptime(median_windowclosedate, format = "%m/%d/%y"),
    opendoy = yday(median_windowopendate),
    closedoy = yday(median_windowclosedate)
  )


# 1. Get mean march daily maximum temperatures
marchmeans <- weatherdata %>%
  filter(month == 3) %>%
  group_by(year) %>%
  summarize(marchmean = mean(airtmax))

# 2.a) EDA of climwin windows
# RP climwin window is around 3/15 to 4/23
#weatherdata %>%
#  filter(flagrp == "RP") %>%
#  mutate(DOY = yday(DATE)) %>%
#  arrange(DOY) %>%
#  slice(c(1, n()))

# DP climwin window is around 3/27 to 6/2
#weatherdata %>%
#  filter(flagdp == "DP") %>%
#  mutate(DOY = yday(DATE)) %>%
#  arrange(DOY) %>%
#  slice(c(1, n()))

climwin_windows <-
  tibble(
    wood_type = c("diffuse-porous", "ring-porous"),
    window = c("climwin window: 3/8 - 5/14", "climwin window: 3/5 - 4/5")
  )


# 2.b) Get mean climwin daily maximum temperatures
# RP separately
#climwinmeans_rp <- weatherdata %>%
#  filter(flagrp == "RP") %>%
#  group_by(year) %>%
#  summarize(climwinmean = mean(TMAX)) %>%
#  mutate(wood_type = "ring-porous")
climwinmeans_rp <- weatherdata %>%
  filter(DOY %in% c(climwindows[1,11]:climwindows[1,12])) %>%
  group_by(year) %>%
  summarize(climwinmean = mean(airtmax)) %>%
  mutate(wood_type = "ring-porous")
# DP separately
#climwinmeans_dp <- weatherdata %>%
#  filter(flagdp == "DP") %>%
#  group_by(year) %>%
#  summarize(climwinmean = mean(TMAX)) %>%
#  mutate(wood_type = "diffuse-porous")
climwinmeans_dp <- weatherdata %>%
  filter(DOY %in% c(climwindows[4,11]:climwindows[4,12])) %>% #68:135
  group_by(year) %>%
  summarize(climwinmean = mean(airtmax)) %>%
  mutate(wood_type = "diffuse-porous")

# Combine
climwinmeans <- bind_rows(climwinmeans_rp, climwinmeans_dp)


# 3. Add to growth data
Wood_pheno_table <- Wood_pheno_table %>%
  left_join(marchmeans, by = "year") %>%
  left_join(climwinmeans, by = c("year", "wood_type")) %>%
  left_join(twosevenfive, by = c("tag", "year"))%>%
  left_join(fiftyseventy, by = c("tag", "year")) %>%
  left_join(twofifty, by = c("tag", "year")) %>%
  # Remove other variables
  #select(-c(tot, dbh, max_rate_DOY, max_rate)) %>%
  mutate(
    perc = case_when(
      perc == 0.25 ~ "DOY_25",
      perc == 0.5 ~ "DOY_50",
      perc == 0.75 ~ "DOY_75"
    )
  ) %>%
  arrange(tag, year)
View(Wood_pheno_table)



# Fit multivariate model using climwinmeans ------------------------------------
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
  #filter(tag %in% sample_tags) %>%
  mutate(climwinmean = climwinmean - 10)

# Delete all non-needed columns
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
  labs(x = "Climwin mean temperature (relative to 16°C)", y = "DOY", col = "Percentile", main = "Relationship of DOY versus climwin mean temperature") +
  geom_text(data = climwin_windows, aes(label = window), x = -Inf, y = -Inf, hjust = -0.01, vjust = -0.5, family = "Avenir")
fig6
ggsave(filename = "doc/manuscript/tables_figures/fig6_HarvardForestV1.png", width = 14.7*.7, height = 10.9*.7, plot = fig6)

# Sanity check this plot with regression table intercepts and slopes
posterior_means_fixed_effects

Run full analysis with ncdc data - then full NCDC w/ met tower windows

#Met tower
#V4 wood table
coefficient                               mean    sd `2.5%`   `97.5%`
<chr>                                    <dbl> <dbl>  <dbl>     <dbl>
  1 y1|(Intercept)                         155.    2.25  150.   159.
2 y1|wood_typering-porous                -49.6   2.75  -55.0  -44.3
3 y1|wood_typediffuse-porous:climwinmean  -1.08  0.730  -2.49   0.339
4 y1|wood_typering-porous:climwinmean     -2.96  0.304  -3.54  -2.36
5 y2|(Intercept)                         173.    2.20  169.   178.
6 y2|wood_typering-porous                -32.2   2.82  -37.8  -26.9
7 y2|wood_typediffuse-porous:climwinmean  -0.913 0.668  -2.18   0.406
8 y2|wood_typering-porous:climwinmean     -1.90  0.262  -2.43  -1.40
9 y3|(Intercept)                         192.    2.53  187.   197.
10 y3|wood_typering-porous                -13.2   3.21  -19.5   -6.93
11 y3|wood_typediffuse-porous:climwinmean  -0.777 0.759  -2.27   0.705
12 y3|wood_typering-porous:climwinmean     -0.573 0.300  -1.17   0.00474

#V6 wood table
coefficient                               mean    sd `2.5%`  `97.5%`
<chr>                                    <dbl> <dbl>  <dbl>    <dbl>
  1 y1|(Intercept)                         154.    2.12  150.   158.
2 y1|wood_typering-porous                -50.0   2.63  -55.1  -44.7
3 y1|wood_typediffuse-porous:climwinmean  -1.52  0.766  -3.02  -0.0726
4 y1|wood_typering-porous:climwinmean     -3.05  0.327  -3.70  -2.40
5 y2|(Intercept)                         174.    2.05  170.   178.
6 y2|wood_typering-porous                -34.5   2.68  -39.7  -29.4
7 y2|wood_typediffuse-porous:climwinmean  -1.70  0.708  -3.09  -0.332
8 y2|wood_typering-porous:climwinmean     -1.95  0.309  -2.55  -1.34
9 y3|(Intercept)                         194.    2.43  189.   199.
10 y3|wood_typering-porous                -17.7   3.08  -23.8  -11.8
11 y3|wood_typediffuse-porous:climwinmean  -1.97  0.789  -3.49  -0.443
12 y3|wood_typering-porous:climwinmean     -0.635 0.339  -1.31   0.0291

#V7 (outliers removed from V6)
coefficient                               mean    sd  `2.5%`  `97.5%`
<chr>                                    <dbl> <dbl>   <dbl>    <dbl>
  1 y1|(Intercept)                         155.    2.16  151.    159.
2 y1|wood_typering-porous                -49.9   2.79  -55.6   -44.7
3 y1|wood_typediffuse-porous:climwinmean  -1.30  0.695  -2.68    0.0567
4 y1|wood_typering-porous:climwinmean     -2.79  0.311  -3.38   -2.20
5 y2|(Intercept)                         174.    2.12  170.    179.
6 y2|wood_typering-porous                -34.1   2.78  -39.5   -28.5
7 y2|wood_typediffuse-porous:climwinmean  -1.46  0.576  -2.58   -0.308
8 y2|wood_typering-porous:climwinmean     -1.58  0.248  -2.05   -1.07
9 y3|(Intercept)                         194.    2.36  190.    199.
10 y3|wood_typering-porous                -16.8   3.07  -22.8   -10.8
11 y3|wood_typediffuse-porous:climwinmean  -1.67  0.656  -2.95   -0.362
12 y3|wood_typering-porous:climwinmean     -0.158 0.291  -0.706   0.433

#Bayesian models for other variables ----
woodtable <- subset(Wood_pheno_table, perc == "DOY_25")
total_formulaRP <- "tot ~ wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

maxrateDOY_formulaRP <- "max_rate_DOY ~wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

maxrate_formulaRP <- "max_rate ~ wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

seasonlength_formulaRP <- "seasonlength ~ wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

earlyperiod_formulaRP <- "twentyfive_to_fifty ~ wood_type + wood_type:climwinmean + (1|tag)" %>% as.formula()

mixedmodel_stanlmerRP_total <- stan_lmer(
  formula = total_formulaRP,
  data = woodtable,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_total %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerRP_maxrateDOY <- stan_lmer(
  formula = maxrateDOY_formulaRP,
  data = woodtable,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_maxrateDOY %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerRP_maxrate <- stan_lmer(
  formula = maxrate_formulaRP,
  data = woodtable,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_maxrate %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerRP_seasonlength <- stan_lmer(
  formula = seasonlength_formulaRP,
  data = woodtable,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_seasonlength %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerRP_earlyperiod <- stan_lmer(
  formula = earlyperiod_formulaRP,
  data = woodtable,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_earlyperiod %>%
  tidy(conf.int = TRUE)



timingplot <- timingplot + geom_point(data = aggregates_warm, aes(x = x, y = Group.2, group = Group.1, color = Group.1))+geom_line(linetype = "dotted")
timingplot + geom_point(data = aggregates_cold, aes(x = x, y = Group.2, group =Group.1, color = Group.1))


##### Mixed models using LME4 ----
library(lme4)
library(lmerTest)
df <- subset(Wood_pheno_table, perc == "DOY_25")
df_rp <- subset(Wood_pheno_table, perc == "DOY_25" & wood_type == "ring-porous")
df_dp <- subset(Wood_pheno_table, perc == "DOY_25" & wood_type == "diffuse-porous")

twofive_rp <- subset(Wood_pheno_table, perc =="DOY_25" & wood_type =="ring-porous")
twofive_dp <- subset(Wood_pheno_table, perc =="DOY_25" & wood_type =="diffuse-porous")
fifty_rp <- subset(Wood_pheno_table, perc =="DOY_50" & wood_type =="ring-porous")
fifty_dp <- subset(Wood_pheno_table, perc =="DOY_50" & wood_type =="diffuse-porous")
sevenfive_rp <- subset(Wood_pheno_table, perc =="DOY_75" & wood_type =="ring-porous")
sevenfive_dp <- subset(Wood_pheno_table, perc =="DOY_75" & wood_type =="diffuse-porous")

summary(lmer(DOY ~ climwinmean + (1|sp/tag), data = twofive_dp))
summary(lmer(DOY ~ climwinmean + (1|sp/tag), data = fifty_dp))
summary(lmer(DOY ~ climwinmean + (1|sp/tag), data = sevenfive_dp))

summary(lmer(DOY ~ climwinmean + (1|sp/tag), data = twofive_rp))
summary(lmer(DOY ~ climwinmean + (1|sp/tag), data = fifty_rp))
summary(lmer(DOY ~ climwinmean + (1|sp/tag), data = sevenfive_rp))

#####Total ----

ggplot(df)+
  geom_boxplot(aes(x=as.character(year), y=tot, fill = wood_type))+
  stat_summary(aes(x=as.character(year), y=tot, group = wood_type), fun = "mean", geom = "point", shape = 8, size = 2, fill = "black")+
  geom_point(aes(x=as.character(year), y=climwinmean, color = wood_type, group = wood_type))+
  geom_line(aes(x=as.character(year), y = climwinmean, color = wood_type, group=wood_type))+
  labs(x = "Year", title = "Total Growth / year / wood type")+
  scale_y_continuous( name = expression("Total yearly growth (dbh)"),
                      ylim.prim <- c(0, 25),
                      ylim.sec <- c(0, 25),
                      sec.axis = sec_axis(~ ., name = "Climwin window mean temperature (C)"))
lm(twofive_rp$tot~twofive_rp$climwinmean)
lm(twofive_dp$tot~twofive_dp$climwinmean)

r <- ggplot(data = twofive_rp, aes(x = climwinmean, y = tot, group = year, fill = wood_type))+geom_boxplot()+
  geom_abline(intercept = 3.59638, slope = 0.09448)+labs(title = "Total Growth (dbh)", x = "Climwin Mean", y = "Total Yearly Growth (DBH)")

r+ ggplot(twofive_dp,aes(x = climwinmean, y = tot, group = year, fill = wood_type))+geom_boxplot()+
  geom_abline(intercept = 3.42599 , slope = 0.03013)+labs(title = "Total Growth (dbh)", x = "Climwin Mean", y = "Total Yearly Growth (DBH)")


boxplot(df$tot~df$wood_type, xlab = "Wood type", ylab = "")
summary(lmer(tot~wood_type*year + (1|sp/tag), data = df))



summary(lmer(tot~climwinmean + (1|sp/tag), data = twofive_rp))
plot(twofive_rp$tot~twofive_rp$climwinmean)
abline(lm(twofive_rp$tot~twofive_rp$climwinmean))

summary(lmer(tot~climwinmean + (1|sp/tag), data = twofive_dp))
plot(twofive_dp$tot~twofive_dp$climwinmean)
abline(lm(twofive_dp$tot~twofive_dp$climwinmean))

##### Max rate / max_rate_DOY ----
library(lme4)
library(lmerTest)
summary(lmer(max_rate~climwinmean + (1|sp/tag), data = twofive_rp))
plot(twofive_rp$max_rate~twofive_rp$climwinmean)
abline(lm(twofive_rp$max_rate~twofive_rp$climwinmean))

summary(lmer(max_rate~climwinmean + (1|sp/tag), data = twofive_dp))
plot(twofive_dp$max_rate~twofive_dp$climwinmean)
abline(lm(twofive_dp$max_rate~twofive_dp$climwinmean))

ggplot(df)+
  geom_boxplot(aes(x = as.character(year), y = max_rate, fill = wood_type))+
  stat_summary(aes(x=as.character(year), y=max_rate, group = wood_type), fun = "mean", geom = "point", shape = 8, size = 4, fill = "black")+
  geom_point(aes(x=as.character(year), y=climwinmean/70, color = wood_type, group = wood_type))+
  geom_line(aes(x=as.character(year), y = climwinmean/70, color = wood_type, group=wood_type))+
  labs(x = "Year", title = "Max growth rates")+
  scale_y_continuous( name = expression("Maximum Growth Rate"),
                      ylim.prim <- c(0, 100),
                      ylim.sec <- c(0, 25),
                      sec.axis = sec_axis(~ .*70, name = "Climwin window mean temperature (C)"))

lm(twofive_rp$max_rate~twofive_rp$climwinmean)
lm(twofive_dp$max_rate~twofive_dp$climwinmean)
r <- ggplot(data = twofive_rp, aes(x = climwinmean, y = max_rate, group = year, fill = wood_type))+geom_boxplot()+
  geom_abline(intercept = 0.0527132, slope = -0.0007811)+labs(title = "Max growth rate")
r+ ggplot(twofive_dp,aes(x = climwinmean, y = max_rate, group = year, fill = wood_type))+geom_boxplot()+
  geom_abline(intercept = 0.0659247 , slope = -0.0001822)+labs(title = "Max growth rates")


summary(lmer(max_rate_DOY~climwinmean + (1|sp/tag), data = twofive_rp))
plot(twofive_rp$max_rate_DOY~twofive_rp$climwinmean)
abline(lm(twofive_rp$max_rate_DOY~twofive_rp$climwinmean))

summary(lmer(max_rate_DOY~climwinmean + (1|sp/tag), data = twofive_dp))
plot(twofive_dp$max_rate_DOY~twofive_dp$climwinmean, xlab = "Temp", ylab= "DOY maximum growth rate achieved")
abline(lm(twofive_dp$max_rate_DOY~twofive_dp$climwinmean))


ggplot(df)+
  geom_boxplot(aes(x=as.character(year), y=max_rate_DOY, fill = wood_type))+
  stat_summary(aes(x=as.character(year), y=max_rate_DOY, group = wood_type), fun = "mean", geom = "point", shape = 8, size = 2, fill = "black")+
  geom_point(aes(x=as.character(year), y=climwinmean*13, color = wood_type, group = wood_type))+
  geom_line(aes(x=as.character(year), y = climwinmean*13, color = wood_type, group=wood_type))+
  labs(x = "Year", title = "Max rate DOY")+
  scale_y_continuous( name = expression("Day of Year"),
                      ylim.prim <- c(0, 200),
                      ylim.sec <- c(0, 25),
                      sec.axis = sec_axis(~ ./13, name = "Climwin window mean temperature (C)"))

lm(twofive_rp$max_rate_DOY~twofive_rp$climwinmean)
lm(twofive_dp$max_rate_DOY~twofive_dp$climwinmean)
r <- ggplot(data = twofive_rp, aes(x = climwinmean, y = max_rate_DOY, group = year, fill = wood_type))+geom_boxplot()+
  geom_abline(intercept = 172.570, slope = -1.968)+labs(title = 'Max Rate DOY')
r+ ggplot(twofive_dp,aes(x = climwinmean, y = max_rate_DOY, group = year, fill = wood_type))+geom_boxplot()+
  geom_abline(intercept = 203.3 , slope = -1.7)+labs(title = "Max Rate DOY")



####75-25 model
library(lme4)
library(lmerTest)

#25-75
ggplot(df)+
  geom_boxplot(aes(x = as.character(year), y = seasonlength, fill = wood_type))+
  stat_summary(aes(x=as.character(year), y=seasonlength, group = wood_type), fun = "mean", geom = "point", shape = 8, size = 4, fill = "black")+
  geom_point(aes(x=as.character(year), y=climwinmean*10, color = wood_type, group = wood_type))+
  geom_line(aes(x=as.character(year), y = climwinmean*10, color = wood_type, group=wood_type))+
  labs(x = "Year", title = "Season length (75% growth DOY - 25% growth DOY)")+
  scale_y_continuous( name = expression("Season length (days)"),
                      ylim.prim <- c(0, 200),
                      ylim.sec <- c(0, 25),
                      sec.axis = sec_axis(~ . /10, name = "Climwin window mean temperature (C)"))

lm(twofive_rp$seasonlength~twofive_rp$climwinmean)
lm(twofive_dp$seasonlength~twofive_dp$climwinmean)

r <- ggplot(data = twofive_rp, aes(x = climwinmean, y = seasonlength, group = year, fill = wood_type))+geom_boxplot()+
  geom_abline(intercept = 33.022, slope = 2.484)+labs(title = "Season length RP (75%-25%)", x = "Climwin Mean", y = "Days")

r+ ggplot(twofive_dp,aes(x = climwinmean, y = seasonlength, group = year, fill = wood_type))+geom_boxplot()+
  geom_abline(intercept = 39.202308 , slope = -0.06882)+labs(title = "Season length DP (75%-25%)", x = "Climwin Mean", y = "Days")

summary(aov(seasonlength ~ wood_type, data = df))

summary(lmer(seasonlength ~ climwinmean + (1|sp/tag), data = df_rp))
plot(df_rp$seasonlength~df_rp$climwinmean)
abline(lm(df_rp$seasonlength~df_rp$climwinmean))

summary(lmer(seasonlength ~ climwinmean + (1|sp/tag), data = df_dp))
plot(df_dp$seasonlength~df_dp$climwinmean)
abline(lm(df_dp$seasonlength~df_dp$climwinmean))


#25-50
ggplot(df)+
  geom_boxplot(aes(x = as.character(year), y = twentyfive_to_fifty, fill = wood_type))+
  stat_summary(aes(x=as.character(year), y=twentyfive_to_fifty, color = wood_type), fun = "mean", geom = "point", shape = 8, size = 4, fill = "black")+
  geom_point(aes(x=as.character(year), y=climwinmean*5, color = wood_type, group = wood_type))+
  geom_line(aes(x=as.character(year), y = climwinmean*5, color = wood_type, group=wood_type))+
  labs(x = "Year", title = "Early growth length (50% growth DOY - 25% growth DOY)")+
  scale_y_continuous( name = expression("Season length (days)"),
                      ylim.prim <- c(0, 100),
                      ylim.sec <- c(0, 25),
                      sec.axis = sec_axis(~ . /5, name = "Climwin window mean temperature (C)"))

lm(twofive_rp$twentyfive_to_fifty~twofive_rp$climwinmean)
lm(twofive_dp$twentyfive_to_fifty~twofive_dp$climwinmean)
r <- ggplot(data = twofive_rp, aes(x = climwinmean, y = twentyfive_to_fifty, group = year, fill = wood_type))+geom_boxplot()+
  geom_abline(intercept = 17.289, slope = 1.138)+labs(title = "50%-25%")
r+ ggplot(twofive_dp,aes(x = climwinmean, y = twentyfive_to_fifty, group = year, fill = wood_type))+geom_boxplot()+
  geom_abline(intercept = 19.3780 , slope = -0.0245)+labs(title = "50%-25%")

ggplot(df, aes(x = wood_type, y = twentyfive_to_fifty)) + geom_boxplot()
summary(aov(twentyfive_to_fifty ~ wood_type, data = df))

summary(lmer(twentyfive_to_fifty ~ climwinmean + (1|sp/tag), data = df_rp))
plot(df_rp$twentyfive_to_fifty~df_rp$climwinmean)
abline(lm(df_rp$twentyfive_to_fifty~df_rp$climwinmean))

summary(lmer(twentyfive_to_fifty ~ climwinmean + (1|sp/tag), data = df_dp))
plot(df_dp$twentyfive_to_fifty~df_dp$climwinmean)
abline(lm(df_dp$twentyfive_to_fifty~df_dp$climwinmean))

#50-75
ggplot(df, aes(x = wood_type, y = fifty_to_seventy)) + geom_boxplot()
summary(aov(fifty_to_seventy ~ wood_type, data = df))

summary(lmer(fifty_to_seventy ~ climwinmean + (1|sp/tag), data = df_rp))
plot(df_rp$fifty_to_seventy~df_rp$climwinmean)
abline(lm(df_rp$fifty_to_seventy~df_rp$climwinmean))

summary(lmer(fifty_to_seventy ~ climwinmean + (1|sp/tag), data = df_dp))
plot(df_dp$fifty_to_seventy~df_dp$climwinmean)
abline(lm(df_dp$fifty_to_seventy~df_dp$climwinmean))

###

rmsmallgrowth <- subset(Wood_pheno_table, tot <= 1)
count(rmsmallgrowth, sp)
count(rmsmallgrowth, year)
count(rmsmallgrowth, tag)

rmlargegrowth <- subset(Wood_pheno_table, tot >= 12)
count(rmlargegrowth, sp)
count(rmlargegrowth, year)
count(rmlargegrowth, tag)

rmsmallgrowth$tag_year <- paste(rmsmallgrowth$tag, rmsmallgrowth$year)

Wood_pheno_table$tag_year <- paste(Wood_pheno_table$tag, Wood_pheno_table$year)
Wood_pheno_table <- Wood_pheno_table[!(Wood_pheno_table$tag_year %in% rmsmallgrowth$tag_year),]
