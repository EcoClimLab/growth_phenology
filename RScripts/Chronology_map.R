library(ggplot2)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(dplyr)
library(maps)
library(dplR)
library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(tmap)
library(raster)

chronology_table <- read.csv("doc/manuscript/tables_figures/chronology_table.csv")
chronology_table$uni <- paste0(chronology_table$Location, chronology_table$wood_type)
unichrono <- chronology_table[!duplicated(chronology_table$uni),]
both_wood_type <- unichrono[duplicated(unichrono$Location),]

chronology_table$wood_type <- ifelse(chronology_table$Location %in% both_wood_type$Location, "Both", chronology_table$wood_type)
coordinates(chronology_table) = ~Longitude + Latitude

# Define the map projection
proj4string(chronology_table) <- CRS("+init=epsg:4326")

# convert sp to sf
sfdat <- st_as_sf(chronology_table)

us <- map_data("state")

sfdat$wood_type <- as.factor(sfdat$wood_type)

levels(sfdat$wood_type) <- c("DP", "RP", "Both")

chronology_map <- ggplot(data = sfdat) +
  scale_colour_viridis_d("Wood Type")+
  geom_map(data = us, map = us,aes(x=long, y=lat, map_id=region), fill = "grey")+
  geom_sf( aes(color = wood_type), size = 1)+
  #geom_sf_text(aes(label = group), nudge_x = -1, nudge_y = 0, size = 3, color = "white")+
  coord_sf(xlim = c(-95, -65), ylim = c(34, 50), expand = FALSE)+
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Chronologies used for Tree Ring Analysis", x = "Longitude", y = "Latitude")



png("doc/manuscript/tables_figures/chronology_map.png",units = "in", width = 15, height = 9, res = 1000 )
chronology_map
dev.off()

