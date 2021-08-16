#Script to analyse drought effects on tree core res
#By: Cameron Dow
#Last edit: 8/16/2021
####################################################
library(readr)
library(reshape2)
library(ggplot2)
chronology_table <- read_csv("doc/manuscript/tables_figures/chronology_table.csv")
names(chronology_table)
chronology_table_slopes <- chronology_table[,c(1:9,11,13)] %>%
  rename("Spring Effect" = "Slope April - April:June-July Tmax",
         "Summer Effect" = "Slope June-July - April:June-July Tmax",
         "Interaction Effect" = "Slope April:June-July Tmax")

melt_chrono_slope <- melt(chronology_table_slopes, id.vars = c("Number", "Location", "Species", "Wood Type",
                                                  "CTW Tmin", "CTW Tmax", "Latitude", "Longitude"))
melt_chrono_slope <- melt_chrono_slope[melt_chrono_slope$value >= -0.5,]

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
summary(lm(chronology_table$`Slope April - April:June-July Tmax`~chronology_table$`CTW Tmax`))
summary(lm(chronology_table$`Slope June-July - April:June-July Tmax`~chronology_table$`CTW Tmax`))
summary(lm(chronology_table$`Slope April:June-July Tmax`~chronology_table$Latitude))

png(file = "doc/manuscript/tables_figures/rw_drought_plot.png", width = 6, height = 4,
    pointsize = 12, bg = "transparent", units = "in", res = 600)
ggplot(melt_chrono_slope, aes(x = Latitude, y = value, group = variable, color = variable))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()+
  labs(x = "Latitude", y = "LM Slope value", color = "Temp variable", title = "LM Slope results by Latitude")
dev.off()

ggplot(melt_chrono_slope, aes(x = Latitude, y = value, group = variable, color = variable))+
  geom_point()+
  geom_smooth(se = FALSE)+
  labs(x = "Latitude", y = "LM Slope value", color = "Temp variable", title = "LM Slope results by Latitude")

#Wood type
ggplot(melt_chrono_slope, aes(x = melt_chrono_slope$`CTW Tmax`, y = value, group = interaction(`Wood Type`, variable), color = interaction(`Wood Type`, variable)))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x = "CTW Tmax (C)", y = "LM Slope value", color = "Temp variable", title = "LM Slope results by CTW Tmax")

ggplot(melt_chrono_slope, aes(x = melt_chrono_slope$`CTW Tmax`, y = value, group = interaction(`Wood Type`, variable), color = interaction(`Wood Type`, variable)))+
  geom_point()+
  geom_smooth(se = FALSE)+
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
