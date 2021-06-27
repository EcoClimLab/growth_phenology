library(lubridate)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(patchwork)
#SCBI ----
SCBI_weather <- read.csv("climate data/met_tower_data_sensor2_ncdc_supplemented.csv")
#spei <- read.csv("~/GitHub/Climate/Climate_Data/SPEI/data_downloaded/SCBI/spei_-78.25_38.75.csv")
spei_all_months <- read.csv("~/GitHub/Climate/Climate_Data/SPEI/data_calculated_with_script/spei_all_months.csv")

spei <- spei_all_months[spei_all_months$sites.sitename %in% "Smithsonian_Conservation_Biology_Institute",]
spei$Date <- as.Date(spei$Date, "%m/%d/%Y")
spei$Date <- as.POSIXct(spei$Date, "%y-%m-%d")

spei$year <- year(spei$Date)
spei$month <- month(spei$Date)

spei_scbi <- spei[spei$year %in% unique(SCBI_weather$year.x),]

#June SPEI SCBI ----
#Grab desired monthly SPEI variables - 4, 6, 12 month in this case
spei_4mth <- spei_scbi[,c(6,51,52)]
spei_4mth <- spei_4mth[spei_4mth$month %in% 6,]
spei_4mth <- spei_4mth[,c(1,2)]

# spei_5mth <- spei_scbi[,c(7,51,52)]
# spei_5mth <- spei_5mth[spei_5mth$month %in% 6,]
# spei_5mth <- spei_5mth[,c(1,2)]

spei_6mth <- spei_scbi[,c(8,51,52)]
spei_6mth <- spei_6mth[spei_6mth$month %in% 6,]
spei_6mth <- spei_6mth[,c(1,2)]

# spei_7mth <- spei_scbi[,c(9,51,52)]
# spei_7mth <- spei_7mth[spei_7mth$month %in% 6,]
# spei_7mth <- spei_7mth[,c(1,2)]
#
# spei_8mth <- spei_scbi[,c(10,51,52)]
# spei_8mth <- spei_8mth[spei_8mth$month %in% 6,]
# spei_8mth <- spei_8mth[,c(1,2)]

spei_12mth <- spei_scbi[,c(14,51,52)]
spei_12mth <- spei_12mth[spei_12mth$month %in% 6,]
spei_12mth <- spei_12mth[,c(1,2)]

SCBI_weather_agg <- SCBI_weather[!(is.na(SCBI_weather$cleantmax)),]
SCBI_weather_agg <- aggregate(SCBI_weather_agg$cleantmax, by = list(SCBI_weather_agg$month, SCBI_weather_agg$year.x), FUN = mean)
names(SCBI_weather_agg) <- c("month", "year", "tmax")

#Left join the spei values onto weather aggs by year
SCBI_weather_agg <- left_join(SCBI_weather_agg, spei_4mth, by = "year")
SCBI_weather_agg <- left_join(SCBI_weather_agg, spei_6mth, by = "year")
SCBI_weather_agg <- left_join(SCBI_weather_agg, spei_12mth, by = "year")

names(SCBI_weather_agg) <- c("month", "year", "tmax", "4-mo", "6-mo", "12-mo")

SCBI_plot_april <- SCBI_weather_agg[SCBI_weather_agg$month %in% 4,]
SCBI_plot_april <- melt(SCBI_plot_april, id.vars = c("month", "year", "tmax"))

April_temp_jun <- ggplot(SCBI_plot_april, aes(x = tmax, y = as.numeric(value), color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 25)) +
  labs(x = "", y = "SPEI", color = "SPEI time frame",title = "June SPEI",  subtitle = "April TMAX vs June SPEI - SCBI")

month_4_jun <- SCBI_plot_april[SCBI_plot_april$variable %in% "4-month",]
month_6_jun <- SCBI_plot_april[SCBI_plot_april$variable %in% "6-month",]
month_12_jun <- SCBI_plot_april[SCBI_plot_april$variable %in% "12-month",]

summary(lm(as.numeric(month_4_jun$value)~month_4_jun$tmax)) #p = 0.381
summary(lm(as.numeric(month_6_jun$value)~month_6_jun$tmax)) #p = 0.2572
summary(lm(as.numeric(month_12_jun$value)~month_12_jun$tmax)) #p = 0.7118

# SCBI_plot_may <- SCBI_weather_agg[SCBI_weather_agg$month %in% 5,]
# SCBI_plot_may <- melt(SCBI_plot_may, id.vars = c("month", "year", "tmax"))
#
# may_temp_jun <- ggplot(SCBI_plot_may, aes(x = tmax, y = as.numeric(value), color = variable))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()+
#   theme(legend.position = "none",
#         text = element_text(size = 25)) +
#   labs(x = "May Tmax", y = "SPEI", color = "SPEI time frame", subtitle = "May TMAX vs June SPEI - SCBI")
#
# SCBI_plot_june <- SCBI_weather_agg[SCBI_weather_agg$month %in% 6,]
# SCBI_plot_june <- melt(SCBI_plot_june, id.vars = c("month", "year", "tmax"))
#
# june_temp_jun <- ggplot(SCBI_plot_june, aes(x = tmax, y = as.numeric(value), color = variable))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()+
#   theme(legend.position = "none",
#         text = element_text(size = 25)) +
#   labs(x = "June Tmax", y = "SPEI", color = "SPEI time frame", subtitle = "June TMAX vs June SPEI - SCBI")

####July Spei SCBI ----

spei_4mth <- spei_scbi[,c(6,51,52)]
spei_4mth <- spei_4mth[spei_4mth$month %in% 7,]
spei_4mth <- spei_4mth[,c(1,2)]

# spei_5mth <- spei_scbi[,c(7,51,52)]
# spei_5mth <- spei_5mth[spei_5mth$month %in% 7,]
# spei_5mth <- spei_5mth[,c(1,2)]

spei_6mth <- spei_scbi[,c(8,51,52)]
spei_6mth <- spei_6mth[spei_6mth$month %in% 7,]
spei_6mth <- spei_6mth[,c(1,2)]

# spei_7mth <- spei_scbi[,c(9,51,52)]
# spei_7mth <- spei_7mth[spei_7mth$month %in% 7,]
# spei_7mth <- spei_7mth[,c(1,2)]

# spei_8mth <- spei_scbi[,c(10,51,52)]
# spei_8mth <- spei_8mth[spei_8mth$month %in% 7,]
# spei_8mth <- spei_8mth[,c(1,2)]

spei_12mth <- spei_scbi[,c(14,51,52)]
spei_12mth <- spei_12mth[spei_12mth$month %in% 7,]
spei_12mth <- spei_12mth[,c(1,2)]

SCBI_weather_agg <- SCBI_weather[!(is.na(SCBI_weather$cleantmax)),]
SCBI_weather_agg <- aggregate(SCBI_weather_agg$cleantmax, by = list(SCBI_weather_agg$month, SCBI_weather_agg$year.x), FUN = mean)
names(SCBI_weather_agg) <- c("month", "year", "tmax")


SCBI_weather_agg <- left_join(SCBI_weather_agg, spei_4mth, by = "year")
SCBI_weather_agg <- left_join(SCBI_weather_agg, spei_6mth, by = "year")
SCBI_weather_agg <- left_join(SCBI_weather_agg, spei_12mth, by = "year")

names(SCBI_weather_agg) <- c("month", "year", "tmax", "4-mo", "6-mo", "12-mo")

SCBI_plot_april <- SCBI_weather_agg[SCBI_weather_agg$month %in% 4,]
SCBI_plot_april <- melt(SCBI_plot_april, id.vars = c("month", "year", "tmax"))

April_temp_jul <- ggplot(SCBI_plot_april, aes(x = tmax, y = as.numeric(value), color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()+
  theme(legend.position = "top",
        legend.title=element_text(size=16),
        text = element_text(size = 25)) +
  labs(x = "", y = "", color = "SPEI time frame", title = "July SPEI" , subtitle = "April TMAX vs July SPEI - SCBI")

month_4_jul <- SCBI_plot_april[SCBI_plot_april$variable %in% "4-month",]
month_6_jul <- SCBI_plot_april[SCBI_plot_april$variable %in% "6-month",]
month_12_jul <- SCBI_plot_april[SCBI_plot_april$variable %in% "12-month",]

summary(lm(as.numeric(month_4_jul$value)~month_4_jul$tmax)) #p = 0.3166
summary(lm(as.numeric(month_6_jul$value)~month_6_jul$tmax)) #p = 0.2166
summary(lm(as.numeric(month_12_jul$value)~month_12_jul$tmax)) #p = 0.7749

# SCBI_plot_may <- SCBI_weather_agg[SCBI_weather_agg$month %in% 5,]
# SCBI_plot_may <- melt(SCBI_plot_may, id.vars = c("month", "year", "tmax"))
#
# may_temp_jul <- ggplot(SCBI_plot_may, aes(x = tmax, y = as.numeric(value), color = variable))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()+
#   theme(legend.position = "none",
#         text = element_text(size = 25)) +
#   labs(x = "May Tmax", y = "SPEI", color = "SPEI time frame", subtitle = "May TMAX vs July SPEI - SCBI")
#
# SCBI_plot_june <- SCBI_weather_agg[SCBI_weather_agg$month %in% 6,]
# SCBI_plot_june <- melt(SCBI_plot_june, id.vars = c("month", "year", "tmax"))
#
# june_temp_jul <- ggplot(SCBI_plot_june, aes(x = tmax, y = as.numeric(value), color = variable))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()+
#   theme(legend.position = "none",
#         text = element_text(size = 25)) +
#   labs(x = "June Tmax", y = "SPEI", color = "SPEI time frame", subtitle = "June TMAX vs July SPEI - SCBI")
#

#### August SPEI SCBI ----

spei_4mth <- spei_scbi[,c(6,51,52)]
spei_4mth <- spei_4mth[spei_4mth$month %in% 8,]
spei_4mth <- spei_4mth[,c(1,2)]

# spei_5mth <- spei_scbi[,c(7,51,52)]
# spei_5mth <- spei_5mth[spei_5mth$month %in% 8,]
# spei_5mth <- spei_5mth[,c(1,2)]

spei_6mth <- spei_scbi[,c(8,51,52)]
spei_6mth <- spei_6mth[spei_6mth$month %in% 8,]
spei_6mth <- spei_6mth[,c(1,2)]

# spei_7mth <- spei_scbi[,c(9,51,52)]
# spei_7mth <- spei_7mth[spei_7mth$month %in% 8,]
# spei_7mth <- spei_7mth[,c(1,2)]
#
# spei_8mth <- spei_scbi[,c(10,51,52)]
# spei_8mth <- spei_8mth[spei_8mth$month %in% 8,]
# spei_8mth <- spei_8mth[,c(1,2)]

spei_12mth <- spei_scbi[,c(14,51,52)]
spei_12mth <- spei_12mth[spei_12mth$month %in% 8,]
spei_12mth <- spei_12mth[,c(1,2)]

SCBI_weather_agg <- SCBI_weather[!(is.na(SCBI_weather$cleantmax)),]
SCBI_weather_agg <- aggregate(SCBI_weather_agg$cleantmax, by = list(SCBI_weather_agg$month, SCBI_weather_agg$year.x), FUN = mean)
names(SCBI_weather_agg) <- c("month", "year", "tmax")


SCBI_weather_agg <- left_join(SCBI_weather_agg, spei_4mth, by = "year")
SCBI_weather_agg <- left_join(SCBI_weather_agg, spei_6mth, by = "year")
SCBI_weather_agg <- left_join(SCBI_weather_agg, spei_12mth, by = "year")

names(SCBI_weather_agg) <- c("month", "year", "tmax", "4-month", "6-month", "12-month")

SCBI_plot_april <- SCBI_weather_agg[SCBI_weather_agg$month %in% 4,]
SCBI_plot_april <- melt(SCBI_plot_april, id.vars = c("month", "year", "tmax"))

April_temp_aug <- ggplot(SCBI_plot_april, aes(x = tmax, y = as.numeric(value), color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 25)) +
  labs(x = "", y = "", color = "SPEI time frame", title = "August SPEI", subtitle = "April TMAX vs August SPEI - SCBI")

month_4_aug <- SCBI_plot_april[SCBI_plot_april$variable %in% "4-month",]
month_6_aug <- SCBI_plot_april[SCBI_plot_april$variable %in% "6-month",]
month_12_aug <- SCBI_plot_april[SCBI_plot_april$variable %in% "12-month",]

summary(lm(as.numeric(month_4_aug$value)~month_4_aug$tmax)) #p = 0.3571
summary(lm(as.numeric(month_6_aug$value)~month_6_aug$tmax)) #p = 0.3703
summary(lm(as.numeric(month_12_aug$value)~month_12_aug$tmax)) #p = 0.9067

# SCBI_plot_may <- SCBI_weather_agg[SCBI_weather_agg$month %in% 5,]
# SCBI_plot_may <- melt(SCBI_plot_may, id.vars = c("month", "year", "tmax"))
#
# may_temp_aug <- ggplot(SCBI_plot_may, aes(x = tmax, y = as.numeric(value), color = variable))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()+
#   theme(legend.position = "none",
#         text = element_text(size = 25)) +
#   labs(x = "May Tmax", y = "SPEI", color = "SPEI time frame", subtitle = "May TMAX vs August SPEI - SCBI")
#
# SCBI_plot_june <- SCBI_weather_agg[SCBI_weather_agg$month %in% 6,]
# SCBI_plot_june <- melt(SCBI_plot_june, id.vars = c("month", "year", "tmax"))
#
# june_temp_aug <- ggplot(SCBI_plot_june, aes(x = tmax, y = as.numeric(value), color = variable))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()+
#   theme(legend.position = "none",
#         text = element_text(size = 25)) +
#   labs(x = "June Tmax", y = "SPEI", color = "SPEI time frame", subtitle = "June TMAX vs August SPEI - SCBI")
#
#
#
# month_4 <- SCBI_plot_april[SCBI_plot_april$variable %in% "4-month",]
# month_6 <- SCBI_plot_april[SCBI_plot_april$variable %in% "6-month",]
# month_12 <- SCBI_plot_april[SCBI_plot_april$variable %in% "12-month",]
#
# summary(lm(as.numeric(month_4$value)~month_4$tmax))
# summary(lm(as.numeric(month_6$value)~month_6$tmax))
# summary(lm(as.numeric(month_12$value)~month_12$tmax))

#Harvard Forest----
weatherdata_hf <-
  read_csv("climate data/HF_weatherdata.csv") %>%
  filter(!is.na(airtmax))#spei <- read.csv("~/GitHub/Climate/Climate_Data/SPEI/data_downloaded/SCBI/spei_-78.25_38.75.csv")

#spei_all_months <- read.csv("~/GitHub/Climate/Climate_Data/SPEI/data_calculated_with_script/spei_all_months.csv")

spei <- spei_all_months[spei_all_months$sites.sitename %in% "Harvard_Forest",]
spei$Date <- as.Date(spei$Date, "%m/%d/%Y")
spei$Date <- as.POSIXct(spei$Date, "%y-%m-%d")

spei$year <- year(spei$Date)
spei$month <- month(spei$Date)

spei_hf <- spei[spei$year %in% unique(weatherdata_hf$year),]

#June SPEI HF ----
#Grab desired monthly SPEI variables - 4, 6, 12 month in this case
spei_4mth <- spei_hf[,c(6,51,52)]
spei_4mth <- spei_4mth[spei_4mth$month %in% 6,]
spei_4mth <- spei_4mth[,c(1,2)]

# spei_5mth <- spei_scbi[,c(7,51,52)]
# spei_5mth <- spei_5mth[spei_5mth$month %in% 6,]
# spei_5mth <- spei_5mth[,c(1,2)]

spei_6mth <- spei_hf[,c(8,51,52)]
spei_6mth <- spei_6mth[spei_6mth$month %in% 6,]
spei_6mth <- spei_6mth[,c(1,2)]

# spei_7mth <- spei_scbi[,c(9,51,52)]
# spei_7mth <- spei_7mth[spei_7mth$month %in% 6,]
# spei_7mth <- spei_7mth[,c(1,2)]
#
# spei_8mth <- spei_scbi[,c(10,51,52)]
# spei_8mth <- spei_8mth[spei_8mth$month %in% 6,]
# spei_8mth <- spei_8mth[,c(1,2)]

spei_12mth <- spei_hf[,c(14,51,52)]
spei_12mth <- spei_12mth[spei_12mth$month %in% 6,]
spei_12mth <- spei_12mth[,c(1,2)]

hf_weather_agg <- weatherdata_hf[!(is.na(weatherdata_hf$airtmax)),]
hf_weather_agg <- aggregate(hf_weather_agg$airtmax, by = list(hf_weather_agg$month, hf_weather_agg$year), FUN = mean)
names(hf_weather_agg) <- c("month", "year", "tmax")

#Left join the spei values onto weather aggs by year
hf_weather_agg <- left_join(hf_weather_agg, spei_4mth, by = "year")
hf_weather_agg <- left_join(hf_weather_agg, spei_6mth, by = "year")
hf_weather_agg <- left_join(hf_weather_agg, spei_12mth, by = "year")

names(hf_weather_agg) <- c("month", "year", "tmax", "4-month", "6-month", "12-month")

hf_plot_april <- hf_weather_agg[hf_weather_agg$month %in% 4,]
hf_plot_april <- melt(hf_plot_april, id.vars = c("month", "year", "tmax"))

April_temp_jun_HF <- ggplot(hf_plot_april, aes(x = tmax, y = as.numeric(value), color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 25)) +
  labs(x = "April Tmax", y = "SPEI", color = "SPEI time frame",title = "",  subtitle = "April TMAX vs June SPEI - HF")

month_4_jun <- hf_plot_april[hf_plot_april$variable %in% "4-month",]
month_6_jun <- hf_plot_april[hf_plot_april$variable %in% "6-month",]
month_12_jun <- hf_plot_april[hf_plot_april$variable %in% "12-month",]

summary(lm(as.numeric(month_4_jun$value)~month_4_jun$tmax)) #p = 0.858
summary(lm(as.numeric(month_6_jun$value)~month_6_jun$tmax)) #p = 0.7189
summary(lm(as.numeric(month_12_jun$value)~month_12_jun$tmax)) #p = 0.4282

# hf_plot_may <- hf_weather_agg[hf_weather_agg$month %in% 5,]
# hf_plot_may <- melt(hf_plot_may, id.vars = c("month", "year", "tmax"))
#
# may_temp_jun <- ggplot(hf_plot_may, aes(x = tmax, y = as.numeric(value), color = variable))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()+
#   theme(legend.position = "none",
#         text = element_text(size = 25)) +
#   labs(x = "May Tmax", y = "SPEI", color = "SPEI time frame", subtitle = "May TMAX vs June SPEI - hf")
#
# hf_plot_june <- hf_weather_agg[hf_weather_agg$month %in% 6,]
# hf_plot_june <- melt(hf_plot_june, id.vars = c("month", "year", "tmax"))
#
# june_temp_jun <- ggplot(hf_plot_june, aes(x = tmax, y = as.numeric(value), color = variable))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()+
#   theme(legend.position = "none",
#         text = element_text(size = 25)) +
#   labs(x = "June Tmax", y = "SPEI", color = "SPEI time frame", subtitle = "June TMAX vs June SPEI - hf")

####July Spei HF ----

spei_4mth <- spei_hf[,c(6,51,52)]
spei_4mth <- spei_4mth[spei_4mth$month %in% 7,]
spei_4mth <- spei_4mth[,c(1,2)]

# spei_5mth <- spei_hf[,c(7,51,52)]
# spei_5mth <- spei_5mth[spei_5mth$month %in% 7,]
# spei_5mth <- spei_5mth[,c(1,2)]

spei_6mth <- spei_hf[,c(8,51,52)]
spei_6mth <- spei_6mth[spei_6mth$month %in% 7,]
spei_6mth <- spei_6mth[,c(1,2)]

# spei_7mth <- spei_hf[,c(9,51,52)]
# spei_7mth <- spei_7mth[spei_7mth$month %in% 7,]
# spei_7mth <- spei_7mth[,c(1,2)]

# spei_8mth <- spei_hf[,c(10,51,52)]
# spei_8mth <- spei_8mth[spei_8mth$month %in% 7,]
# spei_8mth <- spei_8mth[,c(1,2)]

spei_12mth <- spei_hf[,c(14,51,52)]
spei_12mth <- spei_12mth[spei_12mth$month %in% 7,]
spei_12mth <- spei_12mth[,c(1,2)]

hf_weather_agg <- weatherdata_hf[!(is.na(weatherdata_hf$airtmax)),]
hf_weather_agg <- aggregate(hf_weather_agg$airtmax, by = list(hf_weather_agg$month, hf_weather_agg$year), FUN = mean)
names(hf_weather_agg) <- c("month", "year", "tmax")


hf_weather_agg <- left_join(hf_weather_agg, spei_4mth, by = "year")
hf_weather_agg <- left_join(hf_weather_agg, spei_6mth, by = "year")
hf_weather_agg <- left_join(hf_weather_agg, spei_12mth, by = "year")

names(hf_weather_agg) <- c("month", "year", "tmax", "4-month", "6-month", "12-month")

hf_plot_april <- hf_weather_agg[hf_weather_agg$month %in% 4,]
hf_plot_april <- melt(hf_plot_april, id.vars = c("month", "year", "tmax"))

April_temp_jul_HF <- ggplot(hf_plot_april, aes(x = tmax, y = as.numeric(value), color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()+
  theme(legend.position = "none",
        legend.title=element_text(size=14),
        text = element_text(size = 25)) +
  labs(x = "April Tmax", y = "", color = "SPEI time frame", title = "" , subtitle = "April TMAX vs July SPEI - HF")

month_4_jul <- hf_plot_april[hf_plot_april$variable %in% "4-month",]
month_6_jul <- hf_plot_april[hf_plot_april$variable %in% "6-month",]
month_12_jul <- hf_plot_april[hf_plot_april$variable %in% "12-month",]

summary(lm(as.numeric(month_4_jul$value)~month_4_jul$tmax)) #p = 0.7808
summary(lm(as.numeric(month_6_jul$value)~month_6_jul$tmax)) #p = 0.8641
summary(lm(as.numeric(month_12_jul$value)~month_12_jul$tmax)) #p = 0.3368

# hf_plot_may <- hf_weather_agg[hf_weather_agg$month %in% 5,]
# hf_plot_may <- melt(hf_plot_may, id.vars = c("month", "year", "tmax"))
#
# may_temp_jul <- ggplot(hf_plot_may, aes(x = tmax, y = as.numeric(value), color = variable))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()+
#   theme(legend.position = "none",
#         text = element_text(size = 25)) +
#   labs(x = "May Tmax", y = "SPEI", color = "SPEI time frame", subtitle = "May TMAX vs July SPEI - hf")
#
# hf_plot_june <- hf_weather_agg[hf_weather_agg$month %in% 6,]
# hf_plot_june <- melt(hf_plot_june, id.vars = c("month", "year", "tmax"))
#
# june_temp_jul <- ggplot(hf_plot_june, aes(x = tmax, y = as.numeric(value), color = variable))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   theme_bw()+
#   theme(legend.position = "none",
#         text = element_text(size = 25)) +
#   labs(x = "June Tmax", y = "SPEI", color = "SPEI time frame", subtitle = "June TMAX vs July SPEI - hf")
#

#### August SPEI HF ----

spei_4mth <- spei_hf[,c(6,51,52)]
spei_4mth <- spei_4mth[spei_4mth$month %in% 8,]
spei_4mth <- spei_4mth[,c(1,2)]

# spei_5mth <- spei_hf[,c(7,51,52)]
# spei_5mth <- spei_5mth[spei_5mth$month %in% 8,]
# spei_5mth <- spei_5mth[,c(1,2)]

spei_6mth <- spei_hf[,c(8,51,52)]
spei_6mth <- spei_6mth[spei_6mth$month %in% 8,]
spei_6mth <- spei_6mth[,c(1,2)]

# spei_7mth <- spei_hf[,c(9,51,52)]
# spei_7mth <- spei_7mth[spei_7mth$month %in% 8,]
# spei_7mth <- spei_7mth[,c(1,2)]
#
# spei_8mth <- spei_hf[,c(10,51,52)]
# spei_8mth <- spei_8mth[spei_8mth$month %in% 8,]
# spei_8mth <- spei_8mth[,c(1,2)]

spei_12mth <- spei_hf[,c(14,51,52)]
spei_12mth <- spei_12mth[spei_12mth$month %in% 8,]
spei_12mth <- spei_12mth[,c(1,2)]

hf_weather_agg <- weatherdata_hf[!(is.na(weatherdata_hf$airtmax)),]
hf_weather_agg <- aggregate(hf_weather_agg$airtmax, by = list(hf_weather_agg$month, hf_weather_agg$year), FUN = mean)
names(hf_weather_agg) <- c("month", "year", "tmax")


hf_weather_agg <- left_join(hf_weather_agg, spei_4mth, by = "year")
hf_weather_agg <- left_join(hf_weather_agg, spei_6mth, by = "year")
hf_weather_agg <- left_join(hf_weather_agg, spei_12mth, by = "year")

names(hf_weather_agg) <- c("month", "year", "tmax", "4-month", "6-month", "12-month")

hf_plot_april <- hf_weather_agg[hf_weather_agg$month %in% 4,]
hf_plot_april <- melt(hf_plot_april, id.vars = c("month", "year", "tmax"))

April_temp_aug_HF <- ggplot(hf_plot_april, aes(x = tmax, y = as.numeric(value), color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 25)) +
  labs(x = "April Tmax", y = "", color = "SPEI time frame", title = "", subtitle = "April TMAX vs August SPEI - HF")

month_4_aug <- hf_plot_april[hf_plot_april$variable %in% "4-month",]
month_6_aug <- hf_plot_april[hf_plot_april$variable %in% "6-month",]
month_12_aug <- hf_plot_april[hf_plot_april$variable %in% "12-month",]

summary(lm(as.numeric(month_4_aug$value)~month_4_aug$tmax)) #p = 0.4649
summary(lm(as.numeric(month_6_aug$value)~month_6_aug$tmax)) #p = 0.4679
summary(lm(as.numeric(month_12_aug$value)~month_12_aug$tmax)) #p = 0.1281

#Save plots with patchwork ----
png(
  filename = "doc/manuscript/tables_figures/SPEI_plots.png", width = 19, height = 14,
  pointsize = 20, bg = "transparent", units = "in", res = 600
  #restoreConsole = FALSE
)
# April_temp_jun + April_temp_jul + April_temp_aug +
#   may_temp_jun + may_temp_jul + may_temp_aug +
#   june_temp_jun + june_temp_jul + june_temp_aug +
#   plot_layout(nrow = 3)

April_temp_jun + April_temp_jul + April_temp_aug +
April_temp_jun_HF + April_temp_jul_HF + April_temp_aug_HF +
plot_layout(nrow = 2)

dev.off()

####################
