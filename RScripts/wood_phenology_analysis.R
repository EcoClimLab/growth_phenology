# Load packages and data ----
# Loading tidyverse loads ggplot2, dplyr, readr, and many more
library(tidyverse)
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V4.csv")
#versiontwo <- read_csv("Data/Wood_pheno_table_V2.csv")
#versionone <- read_csv("Data/Wood_pheno_table_V1.csv")

unique(Wood_pheno_table$sp)
mean(Wood_pheno_table$max_rate_DOY) #V1: June 7th, V2: June 7th V3: June 7th
#mean(versionone$max_rate_DOY) #V1: June 7th, V2: June 7th



# Exploratory Data Analysis ----------------------------------------------------
boxplot(Wood_pheno_table$max_rate_DOY~Wood_pheno_table$wood_type)
aggregate(Wood_pheno_table$max_rate_DOY, by = list(Wood_pheno_table$sp), FUN = mean )

#ring
ring <- subset(Wood_pheno_table, wood_type == "ring porous")
mean(ring$max_rate)
ring25 <- subset(ring, perc == .25)
mean(ring25$max_rate_DOY)
mean(ring25$DOY)

ring50 <- subset(ring, perc == .50)
mean(ring50$DOY)

ring75 <- subset(ring, perc == .75)
mean(ring75$DOY)

#diffuse
diffuse <- subset(Wood_pheno_table, wood_type == "diffuse-porous")
mean(diffuse$max_rate_DOY)
mean(diffuse$max_rate)
diffuse25 <- subset(diffuse, perc == .25)
mean(diffuse25$DOY)

diffuse50 <- subset(diffuse, perc == .50)
mean(diffuse50$DOY)

diffuse75 <- subset(diffuse, perc == .75)
mean(diffuse75$DOY)
#ring<- subset(twentyfive, wood_type == "ring porous")
#means <- aggregate(diffuse$DOY, by = list(diffuse$year), FUN = mean)
#plot(means$x~means$Group.1)
#diffuse <- subset(twentyfive, wood_type == "diffuse-porous")

#means <- means[5:9, ]
#plot(janmeans$x~means$x)
#25%
twentyfive <- subset(Wood_pheno_table, perc == .25)# & sp == "litu")
boxplot(as.numeric(twentyfive$max_rate_DOY)~twentyfive$sp)
boxplot(as.numeric(twentyfive$DOY)~twentyfive$sp)
boxplot(as.numeric(twentyfive$DOY)~twentyfive$wood_type, xlab = "Wood type", ylab = "Day of Year", main = "Average DOY where 25% growth was achieved")
aggregate(twentyfive$DOY, by = list(twentyfive$wood_type), FUN = mean)
mean(as.numeric(twentyfive$DOY))
sd(as.numeric(twentyfive$DOY))
hist(as.numeric(twentyfive$DOY), breaks = 11, xlab = "DOY", main = "DOY where 25% growth achieved")
#
qual25 <- subset(twentyfive, sp == "qual")
plot(as.numeric(qual25$DOY)~as.numeric(qual25$dbh))
boxplot(as.numeric(qual25$DOY)~qual25$year, xlab = "Year", ylab = "DOY", main = "qual 25% growth")

quru25 <- subset(twentyfive, sp == "quru")
plot(as.numeric(quru25$DOY)~as.numeric(quru25$dbh))
boxplot(as.numeric(quru25$DOY)~quru25$year, xlab = "Year", ylab = "DOY", main = "quru 25% growth")

litu25 <- subset(twentyfive, sp == "litu")
plot(as.numeric(litu25$DOY)~as.numeric(litu25$dbh))
boxplot(as.numeric(litu25$DOY)~litu25$year, xlab = "Year", ylab = "DOY", main = "litu 25% growth")

caco25 <- subset(twentyfive, sp == "caco")
plot(as.numeric(caco25$DOY)~as.numeric(caco25$dbh))
boxplot(as.numeric(caco25$DOY)~caco25$year, xlab = "Year", ylab = "DOY", main = "caco 25% growth")

fagr25 <- subset(twentyfive, sp == "fagr")
plot(as.numeric(fagr25$DOY)~as.numeric(fagr25$dbh))
boxplot(as.numeric(fagr25$DOY)~fagr25$year, xlab = "Year", ylab = "DOY", main = "fagr 25% growth")

juni25 <- subset(twentyfive, sp == "juni")
plot(as.numeric(juni25$DOY)~as.numeric(juni25$dbh))
boxplot(as.numeric(juni25$DOY)~juni25$year, xlab = "Year", ylab = "DOY", main = "juni 25% growth")

tiam25 <- subset(twentyfive, sp == "tiam")
plot(as.numeric(tiam25$DOY)~as.numeric(tiam25$dbh))
boxplot(as.numeric(tiam25$DOY)~tiam25$year , xlab = "Year", ylab = "DOY", main = "tiam 25% growth")

unique(all_stems$sp)
#caca, cofl, nysy

#50%
fifty <- subset(Wood_pheno_table, perc == .50)# & sp == "litu")
boxplot(as.numeric(fifty$DOY)~fifty$sp)
boxplot(as.numeric(fifty$DOY)~fifty$wood_type)
mean(as.numeric(fifty$DOY))
sd(as.numeric(fifty$DOY))
hist(as.numeric(fifty$DOY), breaks = 11, xlab = "DOY", main = "DOY where 50% growth achieved")
#
qual50 <- subset(fifty, sp == "qual")
plot(as.numeric(qual50$DOY)~as.numeric(qual50$dbh))
boxplot(as.numeric(qual50$DOY)~qual50$year, xlab = "Year", ylab = "DOY", main = "qual 50% growth")

quru50 <- subset(fifty, sp == "quru")
plot(as.numeric(quru50$DOY)~as.numeric(quru50$dbh))
boxplot(as.numeric(quru50$DOY)~quru50$year, xlab = "Year", ylab = "DOY", main = "quru 50% growth")

litu50 <- subset(fifty, sp == "litu")
plot(as.numeric(litu50$DOY)~as.numeric(litu50$dbh))
boxplot(as.numeric(litu50$DOY)~litu50$year, xlab = "Year", ylab = "DOY", main = "litu 50% growth")

fagr50 <- subset(fifty, sp == "fagr")
plot(as.numeric(fagr50$DOY)~as.numeric(fagr50$dbh))
boxplot(as.numeric(fagr50$DOY)~fagr50$year, xlab = "Year", ylab = "DOY", main = "fagr 50% growth")

caco50 <- subset(fifty, sp == "caco")
plot(as.numeric(caco50$DOY)~as.numeric(caco50$dbh))
boxplot(as.numeric(caco50$DOY)~caco50$year, xlab = "Year", ylab = "DOY", main = "caco 50% growth")

juni50 <- subset(fifty, sp == "juni")
plot(as.numeric(juni50$DOY)~as.numeric(juni50$dbh))
boxplot(as.numeric(juni50$DOY)~juni50$year, xlab = "Year", ylab = "DOY", main = "juni 50% growth")

tiam50 <- subset(fifty, sp == "tiam")
plot(as.numeric(tiam50$DOY)~as.numeric(tiam50$dbh))
boxplot(as.numeric(tiam50$DOY)~tiam50$year , xlab = "Year", ylab = "DOY", main = "tiam 50% growth")
#75%
seventyfive <- subset(Wood_pheno_table, perc == .75)# & sp == "litu")
boxplot(as.numeric(seventyfive$DOY)~seventyfive$sp)
boxplot(as.numeric(seventyfive$DOY)~seventyfive$wood_type)
mean(as.numeric(seventyfive$DOY))
sd(as.numeric(seventyfive$DOY))
hist(as.numeric(seventyfive$DOY), breaks = 11, xlab = "DOY", main = "DOY where 75% growth achieved")
#
qual75 <- subset(seventyfive, sp == "qual")
plot(as.numeric(qual75$DOY)~as.numeric(qual75$dbh))
boxplot(as.numeric(qual75$DOY)~qual75$year, xlab = "Year", ylab = "DOY", main = "qual 75% growth")

quru75 <- subset(seventyfive, sp == "quru")
plot(as.numeric(quru75$DOY)~as.numeric(quru75$dbh))
boxplot(as.numeric(quru75$DOY)~quru75$year, xlab = "Year", ylab = "DOY", main = "quru 75% growth")

litu75 <- subset(seventyfive, sp == "litu")
plot(as.numeric(litu75$DOY)~as.numeric(litu75$dbh))
boxplot(as.numeric(litu75$DOY)~litu75$year, xlab = "Year", ylab = "DOY", main = "litu 75% growth")

fagr75 <- subset(seventyfive, sp == "fagr")
plot(as.numeric(fagr75$DOY)~as.numeric(fagr75$dbh))
boxplot(as.numeric(fagr75$DOY)~fagr75$year, xlab = "Year", ylab = "DOY", main = "fagr 75% growth")

caco75 <- subset(seventyfive, sp == "caco")
plot(as.numeric(caco75$DOY)~as.numeric(caco75$dbh))
boxplot(as.numeric(caco75$DOY)~caco75$year, xlab = "Year", ylab = "DOY", main = "caco 75% growth")

juni75 <- subset(seventyfive, sp == "juni")
plot(as.numeric(juni75$DOY)~as.numeric(juni75$dbh))
boxplot(as.numeric(juni75$DOY)~juni75$year, xlab = "Year", ylab = "DOY", main = "juni 75% growth")

tiam75 <- subset(seventyfive, sp == "tiam")
plot(as.numeric(tiam75$DOY)~as.numeric(tiam75$dbh))
boxplot(as.numeric(tiam75$DOY)~tiam75$year , xlab = "Year", ylab = "DOY", main = "tiam 75% growth")

plot(meanrate$x~meanrate$Group.1, xlab = "Year", ylab = "Mean maximum rate", main = "Mean maximum growth rates")



# Setting up data for mixed-effect model ---------------------------------------
#Adding CLIMWIN window weather data
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V4.csv")
twentyfive <- subset(Wood_pheno_table, perc == .25)# & sp == "litu")
fifty <- subset(Wood_pheno_table, perc == .50)
seventyfive <- subset(Wood_pheno_table, perc == .75)
#Temperature
#NEON_summary_temp <- read_csv("climate data/NEON_summary_temp.csv")
weatherdata <- read_csv("climate data/NCDC_NOAA_precip_temp.csv")
weatherdata$months <- months(as.Date(weatherdata$DATE))
weatherdata <- weatherdata[complete.cases(weatherdata$flag),]
weatherdata <- weatherdata[complete.cases(weatherdata$flagdp),]
weatherdata <- weatherdata[complete.cases(weatherdata$TMAX),]

#I manually assigned a flag on the rows that correspond to each climwin window
tempmaxmeans <- aggregate(weatherdata$TMAX, by = list(weatherdata$year, weatherdata$flag), FUN = mean)
tempmaxmeansdp <- aggregate(weatherdata$TMAX, by = list(weatherdata$year, weatherdata$flagdp), FUN = mean)

#Temperatures (obsolete) ----
springweather <- subset(weatherdata, months == "January" | months == "February" | months == "March" | months == "April" | months == "May")
springweather <- springweather[complete.cases(springweather$TMAX),]
january <- subset(springweather, months == "January")
janmeans <- aggregate(january$TMAX, by = list(january$year), FUN = mean)
colnames(janmeans) <- c("year", "janmean")

february <- subset(springweather, months == "February")
febmeans <- aggregate(february$TMAX, by = list(february$year), FUN = mean)
colnames(febmeans) <- c("year", "febmean")

march <- subset(springweather, months == "March")
marchmeans <- aggregate(march$TMAX, by = list(march$year), FUN = mean)
colnames(marchmeans) <- c("year", "marchmean")

april <- subset(springweather, months == "April")
aprilmeans <- aggregate(april$TMAX, by = list(april$year), FUN = mean)
colnames(aprilmeans) <- c("year", "aprilmean")

#merge dataframes: Create the ring-porous and diffuse-porous subsets, then append the window data to them ----

#Ring porous
rpmeans <- subset(tempmaxmeans, Group.2 == "RP") #subset the climate means
rpmeans <- rpmeans[c(1:9), c(1,3)] #remove 'flag' row
colnames(rpmeans) <- c("year", "rptemp")

#subset twentyfive DF to only include RP
twentyfiveRP <- subset(twentyfive, wood_type == "ring porous")
twentyfiveRP <- merge(rpmeans, twentyfiveRP, by = "year")

fiftyRP <- subset(fifty, wood_type == "ring porous")
fiftyRP <- merge(fiftyRP, rpmeans, by = "year")

seventyfiveRP <- subset( seventyfive, wood_type == "ring porous")
seventyfiveRP <- merge(seventyfiveRP, rpmeans, by = "year")

#Diffuse porous - repeat of RP process
dpmeans <- subset(tempmaxmeansdp, Group.2 == "DP")
dpmeans <- dpmeans[c(1:9), c(1,3)]
colnames(dpmeans) <- c("year", "dptemp")

twentyfiveDP <- subset(twentyfive, wood_type == "diffuse-porous")
twentyfiveDP <- merge(dpmeans, twentyfiveDP, by = "year")

fiftyDP <- subset(fifty, wood_type == "diffuse-porous")
fiftyDP <- merge(fiftyDP, dpmeans, by = "year")

seventyfiveDP <- subset(seventyfive, wood_type == "diffuse-porous")
seventyfiveDP <- merge(seventyfiveDP, dpmeans, by = "year")

#Check out relationship with plot
dpdoys <- aggregate(twentyfiveDP$DOY, by = list(twentyfiveDP$year), FUN = mean)
plot(dpdoys$x~dpmeans$dptemp)
summary(lm(dpdoys$x~dpmeans$dptemp))

rpdoys <- aggregate(twentyfiveRP$DOY, by = list(twentyfiveRP$year), FUN = mean)
plot(rpdoys$x~rpmeans$rptemp)
summary(lm(rpdoys$x~rpmeans$rptemp))
# Old stuff (remove if you want) ----
colnames(ringporousmeans) <- c("year", "meantemp")
Wood_pheno_table <-  merge(ringporousmeans, Wood_pheno_table, by = "year")
Wood_pheno_table <-  merge(marchmeans, Wood_pheno_table, by = "year")

Wood_pheno_table$marchmean <- as.numeric(Wood_pheno_table$marchmean)
Wood_pheno_table$meantemp <- as.numeric(Wood_pheno_table$meantemp)

Wood_pheno_table$year <- as.character(Wood_pheno_table$year)
twentyfive <- subset(Wood_pheno_table, perc == .25)# & sp == "litu")

twentyfiveRP <- subset(twentyfive, wood_type == "ring porous")

agg <- aggregate(twentyfiveRP$DOY, by = list(twentyfiveRP$year), FUN = mean)

ringporousmeans <- ringporousmeans[c(1:8),]
plot(ringporousmeans$meantemp~agg$x)
summary(lm(agg$x~ringporousmeans$meantemp))
# TODO: Cam adds variables of window_length to twentyfive date frame here:
# NTS: remove wood-type as variable in individual models



# Nested random effects model ------------------------------------------
library(lme4)
library(lmerTest)
#difference according to wood_type
mixedmodelwood_type <- lmer(doy~marchmean + (1|sp/tag), data = Wood_pheno_table)
summary(mixedmodelwood_type)

#DOY using NCDC CLIMWIN data
mixedmodelNCDC <- lmer(DOY~rptemp + (1|sp/tag) , data = twentyfiveRP)
summary(mixedmodelNCDC)

#Max growth rate using NCDC CLIMWIN data
mixedmodelNCDCrate <- lmer(max_rate~rptemp + (1|sp/tag) , data = twentyfiveRP)
summary(mixedmodelNCDCrate)

#Combined into one line, Max rate DOY using NCDC CLIMWIN data
summary(lmer(max_rate_DOY~rptemp + (1|sp/tag) , data = twentyfiveRP))

#Total growth using NCDC CLIMWIN data
summary(lmer(tot~rptemp + (1|sp/tag) , data = twentyfiveRP))

#Diffuse-porous models
#Repeats of RP process using DP data
mixedmodelmarchmeandp <- lmer(DOY~dptemp + (1|sp/tag) , data = twentyfiveDP)
summary(mixedmodelmarchmeandp)
?isSingular
summary(lmer(max_rate~dptemp + (1|sp/tag) , data = twentyfiveDP))

summary(lmer(max_rate_DOY~dptemp + (1|sp/tag) , data = twentyfiveDP))

summary(lmer(tot~dptemp + (1|sp/tag) , data = twentyfiveDP))

####
#mixedmodelrate <- lmer(max_rate_DOY~wood_type + marchmean + wood_type*marchmean + (1|sp:tag) , data = twentyfive)
#summary(mixedmodelrate)



# Non-nested random effects model using glmer2stan interface to stan ----
# library(glmer2stan)
# library(rstan)
# library(StanHeaders)
# stantable <- twentyfive
# stantable$year <- as.integer(as.factor(stantable$year))
# stantable$wood_type <- as.integer(as.factor(stantable$wood_type))
# stantable$tag <- as.integer(as.factor(stantable$tag))
# stantable$sp <- as.integer(as.factor(stantable$sp))
# nwarm = 100
# niter = 400
# chains = 4
# stantable25 <- subset(stantable, perc == .25)
#
# # This is only for random effects model (1|sp), as this does not work for nested
# # random effects model (1|sp/tag)
# stanlm <- lmer2stan(DOY~wood_type*marchmean + (1|sp) , data = stantable25, calcWAIC = T, warmup = nwarm, iter = niter, chains = chains)
#
# print(stanlm)
# stanmer(stanlm)
# plot(stanlm)
# traceplot(stanlm)
# stanlm@stanmodel
#
# boxplot(Wood_pheno_table$tot~Wood_pheno_table$dbh)
# abline(lm(Wood_pheno_table$tot~Wood_pheno_table$dbh))
# summary(lm(Wood_pheno_table$tot~Wood_pheno_table$dbh))
# aggregate(Wood_pheno_table$tot, by = list(Wood_pheno_table$year), FUN = sum)
#
# totals <- aggregate(twentyfive$tot, by = list(twentyfive$year, twentyfive$wood_type), FUN = sum)
# colnames(totals) <- c("Year", "Wood_type", "Total_growth")
# plot <- ggplot(totals, aes(x = Year, y = Total_growth, color = Wood_type))+geom_point()+geom_line(size = 2)+ labs(title = "Total growth over time at SCBI")
# plot



# Random effects model using rstanarm interface to stan ----
library(rstanarm)
# Tools for exploring mixed effects model outputs:
library(broom.mixed)
library(sjPlot)
library(tidybayes)

# Convert input data frames to tibbles and add variable id'ing each row
twentyfive <- twentyfive %>%
  as_tibble() %>%
  mutate(id = 1:n())

twentyfiveRP <- twentyfiveRP %>%
  as_tibble() %>%
  mutate(id = 1:n())

fiftyRP <- fiftyRP %>%
  as_tibble() %>%
  mutate(id = 1:n())

seventyfiveRP <- seventyfiveRP %>%
  as_tibble() %>%
  mutate(id = 1:n())

twentyfiveDP <- twentyfiveDP %>%
  as_tibble() %>%
  mutate(id = 1:n())

fiftyDP<- fiftyDP %>%
  as_tibble() %>%
  mutate(id = 1:n())

seventyfiveDP <- seventyfiveDP %>%
  as_tibble() %>%
  mutate(id = 1:n())

# Model formula option 1: Just tag
DOY_formula <- "DOY ~ wood_type*marchmean + (1|tag)" %>% as.formula()

# Model formula option 2: tag nested within sp
# Reference: https://m-clark.github.io/mixed-models-with-R/extensions.html#hierarchical-structure
DOY_formula <- "DOY ~ wood_type*marchmean + (1|sp) + (1|sp:tag)" %>% as.formula()

# Model formula option 3: tag nested within sp, but set up so that interaction
# parameters individually interpreted, and not baseline vs offset interpreted
# https://www.researchgate.net/post/How_can_I_get_confidence_intervals_for_multiple_slopes_in_R
DOY_formula <- "DOY ~ wood_type + wood_type:marchmean + (1|sp) + (1|sp:tag)" %>% as.formula()


# Fit both lmer and stan_lmer models using same formula
mixedmodel <- lmer(
  formula = DOY_formula,
  data = twentyfive
)

mixedmodel_stanlmer <- stan_lmer(
  formula = DOY_formula,
  # When using stan_lmer, no need to convert categorical variables to integers
  # like with glmer2stan, so we can use original twentyfive instead of stantable25
  data = twentyfive,
  seed = 349,
  # Half of iter are used as burn-in, rest are included in posterior sample
  iter = 4000,
  # number of multicore chains to run
  chains = 2
)

# Ring-porous stan_lmer's ----
DOY_formulaRP <- "DOY ~ rptemp + (1|sp) + (1|sp:tag)" %>% as.formula()

total_formulaRP <- "tot ~ rptemp + (1|sp) + (1|sp:tag)" %>% as.formula()

maxrateDOY_formulaRP <- "max_rate_DOY ~ rptemp + (1|sp) + (1|sp:tag)" %>% as.formula()

maxrate_formulaRP <- "max_rate ~ rptemp + (1|sp) + (1|sp:tag)" %>% as.formula()

mixedmodel_stanlmerRP_doy25 <- stan_lmer( #25
  formula = DOY_formulaRP,
  data = twentyfiveRP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_doy25 %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerRP_doy50 <- stan_lmer( #50
  formula = DOY_formulaRP,
  data = fiftyRP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_doy50 %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerRP_doy75 <- stan_lmer( #75
  formula = DOY_formulaRP,
  data = seventyfiveRP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_doy75 %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerRP_total <- stan_lmer(
  formula = total_formulaRP,
  data = twentyfiveRP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_total %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerRP_maxrateDOY <- stan_lmer(
  formula = maxrateDOY_formulaRP,
  data = twentyfiveRP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_maxrateDOY %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerRP_maxrate <- stan_lmer(
  formula = maxrate_formulaRP,
  data = twentyfiveRP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerRP_maxrate %>%
  tidy(conf.int = TRUE)

#
posterior_draws <- mixedmodel_stanlmerRP_total %>%
  spread_draws(b[,group])


# 1. Posterior distributions of all species random effects parameters: all
# roughly centered at 0. In other words, almost no difference in species
# level intercepts
posterior_draws %>%
  filter(!str_detect(group, "sp:tag:")) %>%
  separate(group, c("group1", "sp"), ":", remove = FALSE) %>%
  ggplot(aes(x = b, col = sp)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Total growth",
    title = "Posterior distributions of RP species random effects parameters",
    col = "species"
  ) +
  coord_cartesian(xlim = c(-10, 10))


mixedmodel_stanlmerRP_doy %>%
  tidy(conf.int = TRUE)
mixedmodel_stanlmerRP_doy  %>%
  sjPlot::tab_model()

mixedmodel_stanlmerRP_total %>%
  tidy(conf.int = TRUE)
mixedmodel_stanlmerRP_total  %>%
  sjPlot::tab_model()

mixedmodel_stanlmerRP_maxrateDOY %>%
  tidy(conf.int = TRUE)
mixedmodel_stanlmerRP_maxrateDOY  %>%
  sjPlot::tab_model()

mixedmodel_stanlmerRP_maxrate %>%
  tidy(conf.int = TRUE)
mixedmodel_stanlmerRP_maxrate  %>%
  sjPlot::tab_model()
# Diffuse-porous stan-lmer's ----
DOY_formulaDP <- "DOY ~ dptemp + (1|sp) + (1|sp:tag)" %>% as.formula()

total_formulaDP <- "tot ~ dptemp + (1|sp) + (1|sp:tag)" %>% as.formula()

maxrateDOY_formulaDP <- "max_rate_DOY ~ dptemp + (1|sp) + (1|sp:tag)" %>% as.formula()

maxrate_formulaDP <- "max_rate ~ dptemp + (1|sp) + (1|sp:tag)" %>% as.formula()

mixedmodel_stanlmerDP_doy25 <- stan_lmer(#25
  formula = DOY_formulaDP,
  data = twentyfiveDP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerDP_doy25 %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerDP_doy50 <- stan_lmer( #50
  formula = DOY_formulaDP,
  data = fiftyDP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerDP_doy50 %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerDP_doy75 <- stan_lmer(#75
  formula = DOY_formulaDP,
  data = seventyfiveDP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerDP_doy75 %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerDP_total <- stan_lmer(
  formula = total_formulaDP,
  data = twentyfiveDP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerDP_total %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerDP_maxrateDOY <- stan_lmer(
  formula = maxrateDOY_formulaDP,
  data = twentyfiveDP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerDP_maxrateDOY %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerDP_maxrate <- stan_lmer(
  formula = maxrate_formulaDP,
  data = twentyfiveDP,
  seed = 349,
  iter = 4000,
  chains = 2
)

mixedmodel_stanlmerDP_maxrate %>%
  tidy(conf.int = TRUE)

mixedmodel_stanlmerDP_doy %>%
  tidy(conf.int = TRUE)
mixedmodel_stanlmerDP_doy  %>%
  sjPlot::tab_model()

mixedmodel_stanlmerDP_total %>%
  tidy(conf.int = TRUE)
mixedmodel_stanlmerDP_total  %>%
  sjPlot::tab_model()

mixedmodel_stanlmerDP_maxrateDOY %>%
  tidy(conf.int = TRUE)
mixedmodel_stanlmerDP_maxrateDOY  %>%
  sjPlot::tab_model()

mixedmodel_stanlmerDP_maxrate %>%
  tidy(conf.int = TRUE)
mixedmodel_stanlmerDP_maxrate  %>%
  sjPlot::tab_model()

#


posterior_draws <- mixedmodel_stanlmerDP_total %>%
  spread_draws(b[,group])

# 1. Posterior distributions of all species random effects parameters: all
# roughly centered at 0. In other words, almost no difference in species
# level intercepts
posterior_draws %>%
  filter(!str_detect(group, "sp:tag:")) %>%
  separate(group, c("group1", "sp"), ":", remove = FALSE) %>%
  ggplot(aes(x = b, col = sp)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Total growth",
    title = "Posterior distributions of DP species random effects parameters",
    col = "species"
  ) +
  coord_cartesian(xlim = c(-10, 10))






# Bert example code 1: Analysis of lmer output ----
# Look at parameter estimates with confidence intervals, two ways.
# Notice how if we use a model formula of form wood_type + wood_type:marchmean,
# and not wood_type*marchmean, we can slopes for marchmean for each of the 3
# wood types directly, no baseline vs offset needed.
mixedmodel %>%
  tidy(conf.int = TRUE)
mixedmodel  %>%
  sjPlot::tab_model()

# Plot parameter estimates with confidence intervals
mixedmodel %>%
  sjPlot::plot_model()
# Focus only on slope for marchmean parameters and add fancy line at 0
mixedmodel %>%
  sjPlot::plot_model(terms = c("wood_typediffuse-porous:marchmean", "wood_typeother:marchmean", "wood_typering porous:marchmean")) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)



# Focus only on slope for marchmean parameters and add fancy line at 0
mixedmodel %>%
  sjPlot::plot_model(terms = c("wood_typediffuse-porous:marchmean", "wood_typeother:marchmean", "wood_typering porous:marchmean")) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)


# Bert example code 2: Analysis of stan_lmer output ----
# Reference: http://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html

# Look at non-random parameter estimates with (Bayesian) credible intervals
# Notice how if we use a model formula of form wood_type + wood_type:marchmean,
# and not wood_type*marchmean, we can slopes for marchmean for each of the 3
# wood types directly, no baseline vs offset needed.
mixedmodel_stanlmer %>%
  tidy(conf.int = TRUE)

# Identify all parameters in model. Not really used anywhere directly.
mixedmodel_stanlmer %>%
  get_variables()

# Get a data frame of all posterior draws. There are iter * chain * {# of random
# effects parameters} of them. This data frame is essential since it is used for
# the rest of the code
posterior_draws_random_effects <- mixedmodel_stanlmerDP_doy %>%
  # No point in including term since there are only intercepts:
  # spread_draws(b[term,group]) %>%
  # So omit it:
  spread_draws(b[,group])

# Pick out only posterior draws related to sp random effects
posterior_draws_sp <- posterior_draws_random_effects %>%
  # Pick out only species random effects:
  filter(!str_detect(group, "sp:tag:")) %>%
  separate(group, c("group1", "sp"), ":", remove = FALSE)

# Pick out only posterior draws related to tag nested within sp random effects
posterior_draws_tag_within_sp <- posterior_draws_random_effects %>%
  # Pick out tag nested within species random effects:
  filter(str_detect(group, "sp:tag:")) %>%
  separate(group, c("group1", "group2", "sp", "tag"), ":", remove = FALSE)


# Bert example plot 1: Posterior distributions of sp random effects ----
# All roughly centered at 0. In other words, almost no difference in
# species level intercepts
ggplot(posterior_draws_sp, aes(x = b, col = sp)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "DOY",
    title = "Posterior distributions of all 7 species random effects parameters",
    col = "species"
  ) +
  coord_cartesian(xlim = c(-10, 10))


# Bert example plot 2: Posterior distributions of tag nested within sp random effects ----
# First identify which tags in above plot have 95% credible intervals that do not
# contain 0 i.e. that deviate the most from 0
conf_level <- 0.95
outlier_tags <- posterior_draws_tag_within_sp %>%
  group_by(sp, tag) %>%
  summarize(
    mean = mean(b),
    lower_ci = quantile(b, probs = (1-conf_level)/2),
    upper_ci = quantile(b, probs = 1-(1-conf_level)/2)
  ) %>%
  mutate(doesnt_contain_zero = upper_ci < 0 | lower_ci > 0 ) %>%
  filter(doesnt_contain_zero) %>%
  pull(tag)
outlier_tags

# Second, identify these outlier_tags in data frame with outlier variable. That
# way we can map this variable to an aesthetic in ggplot
posterior_draws_tag_within_sp <- posterior_draws_tag_within_sp %>%
  mutate(outlier = tag %in% outlier_tags)

# Third, plot:
ggplot(posterior_draws_tag_within_sp, aes(x = b, group = group, col = outlier)) +
  geom_density() +
  scale_color_manual(values = c("black", "orange")) +
  facet_wrap(~sp) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "DOY",
    title = "Posterior distributions of all 109 tags nested within 7 species random effects parameters"
  )



# Bert example plot 3: Show distribution of observed and fitted DOY ----
# Get Bayesian posterior fitted values
DOY_hat_stan_samples <- mixedmodel_stanlmer %>%
  posterior_predict() %>%
  as.matrix()

# Get fitted values:
fitted_DOY <- mixedmodel %>%
  augment() %>%
  mutate(id = 1:n()) %>%
  select(id, DOY, DOY_hat_lmer = .fitted) %>%
  left_join(twentyfive, by = c("id", "DOY")) %>%
  # Get posterior means for stan
  mutate(DOY_hat_stan = DOY_hat_stan_samples %>% apply(2, mean)) %>%
  # Convert to tidy:
  select(starts_with("DOY"), wood_type, marchmean, sp, tag, year) %>%
  pivot_longer(cols = c("DOY", "DOY_hat_lmer", "DOY_hat_stan"), names_to = "DOY_type", values_to = "DOY") %>%
  mutate(outlier = tag %in% outlier_tags)

# Plot. There is shrinkage! Also, I tagged the outlier_tags in orange
ggplot(fitted_DOY, aes(x = sp, y = DOY)) +
  geom_boxplot() +
  geom_point(aes(col = outlier), size = 2) +
  scale_color_manual(values = c("transparent", "orange")) +
  facet_wrap(~DOY_type) +
  labs(x = "Wood type", y = "DOY", title = "DOY 25% growth is achieved: Observed DOY and fitted DOY from two models (all 7 years together)")



# Bert example plot 4: Posterior mean and 95% credible intervals of DOY of all trees ----
# Deep deep in to Bayesian results
DOY_hat_stan_samples_conf_int <- DOY_hat_stan_samples %>%
  t() %>%
  as_tibble() %>%
  mutate(id = 1:n()) %>%
  left_join(twentyfive, by = c("id")) %>%
  select(tag, starts_with("V")) %>%
  pivot_longer(cols = starts_with("V"), names_to = "sim", values_to = "DOY") %>%
  group_by(tag) %>%
  summarize(
    post_mean = mean(DOY),
    lower_ci = quantile(DOY, probs = 0.025),
    upper_ci = quantile(DOY, probs = 0.975),
    width = upper_ci - lower_ci
  ) %>%
  left_join(twentyfive, by = "tag")  %>%
  arrange(wood_type, post_mean) %>%
  mutate(id = 1:n())

# Plot. Posterior mean and 95% credible intervals of DOY of all trees
ggplot(DOY_hat_stan_samples_conf_int) +
  geom_segment(aes(y = id, yend = id, x = lower_ci, xend = upper_ci, col = wood_type), alpha = 0.2) +
  geom_point(aes(y=id, x = post_mean, col = wood_type)) +
  labs(x = "DOY", y = "", title = "95% credible interval of DOY (all 7 years)")
