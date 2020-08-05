# Load packages and data ----
# Loading tidyverse loads ggplot2, dplyr, readr, and many more
library(tidyverse)
Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V4.csv")
versiontwo <- read_csv("Data/Wood_pheno_table_V2.csv")
versionone <- read_csv("Data/Wood_pheno_table_V1.csv")

unique(Wood_pheno_table$sp)
mean(Wood_pheno_table$max_rate_DOY) #V1: June 7th, V2: June 7th V3: June 7th
#mean(versionone$max_rate_DOY) #V1: June 7th, V2: June 7th



# Exploratory Data Analysis ----------------------------------------------------
boxplot(Wood_pheno_table$max_rate_DOY~Wood_pheno_table$wood_type)
aggregate(Wood_pheno_table$max_rate_DOY, by = list(Wood_pheno_table$sp), FUN = mean )

ring<- subset(twentyfive, wood_type == "ring porous")
means <- aggregate(diffuse$DOY, by = list(diffuse$year), FUN = mean)
plot(means$x~means$Group.1)
diffuse <- subset(twentyfive, wood_type == "diffuse-porous")

means <- means[5:9, ]
plot(janmeans$x~means$x)
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

Wood_pheno_table <- read_csv("Data/Wood_pheno_table_V4.csv")
twentyfive <- subset(Wood_pheno_table, perc == .25)# & sp == "litu")

#Temperature
#NEON_summary_temp <- read_csv("climate data/NEON_summary_temp.csv")
weatherdata <- read_csv("climate data/NCDC_NOAA_precip_temp.csv")
weatherdata$months <- months(as.Date(weatherdata$DATE))

#Temperatures
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

Wood_pheno_table <-  merge(marchmeans, Wood_pheno_table, by = "year")
Wood_pheno_table$marchmean <- as.numeric(Wood_pheno_table$marchmean)
Wood_pheno_table$year <- as.character(Wood_pheno_table$year)
twentyfive <- subset(Wood_pheno_table, perc == .25)# & sp == "litu")



# Nested random effects model ------------------------------------------
library(lme4)
library(lmerTest)
mixedmodel <- lmer(DOY~wood_type*marchmean + (1|sp/tag) , data = twentyfive)
summary(mixedmodel)

mixedmodelrate <- lmer(max_rate_DOY~wood_type + marchmean + wood_type*marchmean + (1|sp:tag) , data = twentyfive)
summary(mixedmodelrate)



# Non-nested random effects model using glmer2stan interface to stan ----
library(glmer2stan)
library(rstan)
library(StanHeaders)
stantable <- twentyfive
stantable$year <- as.integer(as.factor(stantable$year))
stantable$wood_type <- as.integer(as.factor(stantable$wood_type))
stantable$tag <- as.integer(as.factor(stantable$tag))
stantable$sp <- as.integer(as.factor(stantable$sp))
nwarm = 100
niter = 400
chains = 4
stantable25 <- subset(stantable, perc == .25)

# This is only for random effects model (1|sp), as this does not work for nested
# random effects model (1|sp/tag)
stanlm <- lmer2stan(DOY~wood_type*marchmean + (1|sp) , data = stantable25, calcWAIC = T, warmup = nwarm, iter = niter, chains = chains)

print(stanlm)
stanmer(stanlm)
plot(stanlm)
traceplot(stanlm)
stanlm@stanmodel

boxplot(Wood_pheno_table$tot~Wood_pheno_table$dbh)
abline(lm(Wood_pheno_table$tot~Wood_pheno_table$dbh))
summary(lm(Wood_pheno_table$tot~Wood_pheno_table$dbh))
aggregate(Wood_pheno_table$tot, by = list(Wood_pheno_table$year), FUN = sum)

totals <- aggregate(twentyfive$tot, by = list(twentyfive$year, twentyfive$wood_type), FUN = sum)
colnames(totals) <- c("Year", "Wood_type", "Total_growth")
plot <- ggplot(totals, aes(x = Year, y = Total_growth, color = Wood_type))+geom_point()+geom_line(size = 2)+ labs(title = "Total growth over time at SCBI")
plot



# Non-nested random effects model using rstanarm interface to stan ----
library(rstanarm)
# Used for "tidying" mixed effects model outputs:
library(broom.mixed)

# Convert to tibble for easier viewing
twentyfive <- twentyfive %>%
  as_tibble()
stantable25 <- stantable25 %>%
  as_tibble()

# Use common formula
DOY_formula <- "DOY ~ wood_type*marchmean + (1|sp)" %>% as.formula()

mixedmodel <- lmer(
  formula = DOY_formula,
  data = twentyfive
)
mixedmodel_stanlmer <- stan_lmer(
  formula = DOY_formula,
  data = stantable25,
  seed = 349
)

# Compare summary of model outputs
mixedmodel %>% tidy()
mixedmodel_stanlmer %>% tidy()

# Get fitted values:
fitted_DOY <- mixedmodel %>%
  augment() %>%
  # Get fitted values for lmer
  rename(
    DOY_observed = DOY,
    DOY_hat_lmer = .fitted
  ) %>%
  # Get posterior means for stan
  mutate(DOY_hat_stan = mixedmodel_stanlmer %>% posterior_predict() %>% apply(2, mean)) %>%
  # Convert to tidy:
  select(starts_with("DOY"), wood_type, marchmean, sp) %>%
  pivot_longer(cols = c("DOY_observed", "DOY_hat_lmer", "DOY_hat_stan"), names_to = "DOY_type", values_to =)

# There is shrinkage!
ggplot(fitted_DOY, aes(x = wood_type, y = value)) +
  geom_boxplot() +
  facet_wrap(~DOY_type)

# There is shrinkage!
ggplot(fitted_DOY, aes(x = sp, y = value)) +
  geom_boxplot() +
  facet_wrap(~DOY_type)


