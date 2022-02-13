
# Title:      Dartmoor vector vs raster
# Objective:  Make contour and raster plots of Dartmoor
# Created by: Pia Benaud
# Created on: 18/01/2022


# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(elevatr)
library(raster)
library(MetBrewer)
library(rayshader)


# Setup -------------------------------------------------------------------
Dartmoor_sf <- st_read("Data/NE_NationalParksEngland_SHP/data/National_Parks_England.shp")

Dartmoor_DEM <- get_elev_raster(Dartmoor_sf, z = 11, clip = "location", neg_to_na = "TRUE")

Dartmoor_DEM_DF <- as.data.frame(Dartmoor_DEM, xy = TRUE) %>% 
  rename_with(.cols = 3, ~"elevation") %>% 
  drop_na(elevation) 


# Main plot function ------------------------------------------------------
main_plot <- function(shp){
  ggplot() +
    geom_sf(data = st_graticule(shp, 
                                lat = seq(50.4, 50.7, by = 0.1),
                                lon = seq(-4.1, -3.7, by = 0.1)), 
            color = 'gray30') +
    geom_sf(data = shp, colour = "black", fill = "white", size = 1) +
    coord_sf(label_graticule = "NESW") +
    scale_y_continuous(breaks = seq(50.4, 50.7, by = 0.1)) + 
    scale_x_continuous(breaks = seq(-4.1, -3.7, by = 0.1)) +
    labs(title = "DARTMOOR",
         subtitle = "- National Park -",
         caption = "Vis: @piabenaud | Data: STRM, © Natural England copyright. Contains Ordnance Survey data © Crown copyright") +
    theme_classic() +
    theme(legend.position="none") +
    theme(axis.title = element_blank(),
          axis.text = element_text(family = "Amatic SC Bold",
                                   colour = "black",
                                   size = 10),
          plot.title = element_text(hjust = 0.5,
                                    family = "Amatic SC Bold",
                                    size = 50,
                                    colour = "black",
                                    margin = margin(30, 0, 0, 0)),
          plot.subtitle = element_text(hjust = 0.5,
                                       family = "Amatic SC",
                                       size = 25,
                                       colour = "black",
                                       margin = margin(5, 0, 20, 0)),
          plot.caption = element_text(hjust = 0.5,
                                      family = "Amatic SC",
                                      colour = "grey20",
                                      size = 8,
                                      margin = margin(10, 0, 5, 0)))
}


# Make the contour plot ---------------------------------------------------
dartmoor_map_vect <- main_plot(Dartmoor_sf) +
  geom_contour(data = Dartmoor_DEM_DF, 
               aes(x = x, y = y, z = elevation, colour = after_stat(level)),
               binwidth = 10, 
               size = 0.2) +
  scale_color_gradientn(colours = rev(met.brewer("Hokusai2")))

ggsave(file="Figures/Dart_vect.png", plot=dartmoor_map_vect, width=8, height=8)


# Make the raster plot ----------------------------------------------------
dartmoor_map_rast <- main_plot(Dartmoor_sf) +
  geom_raster(data = Dartmoor_DEM_DF, 
              aes(x = x, y = y, fill = elevation))+
  scale_fill_gradientn(colours = rev(met.brewer("Hokusai2")))

ggsave(file="Figures/Dart_rast.png", plot=dartmoor_map_rast, width=8, height=8)


# Rayshader it! -----------------------------------------------------------
plot_gg(dartmoor_map_rast, width = 5, height = 5, multicore = TRUE,
        zoom = 0.7, phi = 70, theta = 0, windowsize = c(800, 800))
Sys.sleep(0.2)
render_snapshot("Figures/dart_rast_ray.png")
