#Script to analyse drought effects on tree core res
#By: Cameron Dow
#Last edit: 7/18/2021
####################################################
library(readr)
library(reshape2)
library(ggplot2)
chronology_table <- read_csv("doc/manuscript/tables_figures/chronology_table.csv")
names(chronology_table)
chronology_table_slopes <- chronology_table[,c(1:9,11,13,15,17,19,21,23,25,27)]
melt_chrono_slope <- melt(chronology_table_slopes, id.vars = c("Number", "Location", "Species", "Wood Type",
                                                  "CTW Tmin", "CTW Tmax", "Latitude", "Longitude"))
#TMAX
ggplot(melt_chrono_slope, aes(x = melt_chrono_slope$`CTW Tmax`, y = value, group = variable, color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "CTW Tmax (C)", y = "LM Slope value", color = "Temp variable", title = "LM Slope results by CTW Tmax")
#TMIN
ggplot(melt_chrono_slope, aes(x = melt_chrono_slope$`CTW Tmin`, y = value, group = variable, color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "CTW Tmin (C)", y = "LM Slope value", color = "Temp variable", title = "LM Slope results by CTW Tmin")
#Latitude
ggplot(melt_chrono_slope, aes(x = Latitude, y = value, group = variable, color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "Latitude", y = "LM Slope value", color = "Temp variable", title = "LM Slope results by Latitude")

ggplot(melt_chrono_slope, aes(x = Latitude, y = value, group = variable, color = variable))+
  geom_point()+
  geom_smooth(se = FALSE)+
  labs(x = "Latitude", y = "LM Slope value", color = "Temp variable", title = "LM Slope results by Latitude")

#Wood type
ggplot(melt_chrono_slope, aes(x = melt_chrono_slope$`CTW Tmax`, y = value, group = interaction(`Wood Type`, variable), color = interaction(`Wood Type`, variable)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "CTW Tmax (C)", y = "LM Slope value", color = "Temp variable", title = "LM Slope results by CTW Tmax")

#chronology - only interactions
chronology_table_interactions <- chronology_table[,c(1:9,11,15,19)]
melt_chrono_interactions <- melt(chronology_table_interactions, id.vars = c("Number", "Location", "Species", "Wood Type",
                                                               "CTW Tmin", "CTW Tmax", "Latitude", "Longitude"))
ggplot(melt_chrono_interactions, aes(x = Latitude, y = value, group = variable, color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "Latitude", y = "LM Slope value", color = "Temp variable", title = "LM Slope results by Latitude")

#Check how april effects change according to model
chronology_table_apr_slopes <- chronology_table[,c(1:9,11,17,23)]
melt_chrono_apr_slope <- melt(chronology_table_apr_slopes, id.vars = c("Number", "Location", "Species", "Wood Type",
                                                               "CTW Tmin", "CTW Tmax", "Latitude", "Longitude"))
ggplot(melt_chrono_apr_slope, aes(x = variable, y = value,color = as.factor(Number), group= as.factor(Number))) +
  geom_point()+
  geom_smooth( se = FALSE)+
  theme(legend.position = "none")
