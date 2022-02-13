
# Title:      Shapefile MapViewer
# Objective:  Have a quick look at shps on a map
# Created by: Pia Benaud
# Created on: 11-02-2022


# Load packages -----------------------------------------------------------

library(sf) # for dealing with the shapefiles
library(dplyr) # wrangling data into a usable format
library(leaflet) # main map

#rm(list = ls()) # if you need a quick clean of the environment - DOES NOT UNLOAD PACKAGES!


# Mapping Function --------------------------------------------------------

shp_viewer <- function(the_sf, geom_number = 1){
  
  centres <- st_coordinates(st_centroid(the_sf))
  
  leaflet() %>%  
    setView(lng = centres[geom_number,1], lat = centres[geom_number,2], zoom = 12) %>% 
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
    addProviderTiles(providers$Esri.WorldImagery,
                     options = providerTileOptions(opacity = 0.7)) %>% 
    addPolygons(data = Peat_UAV_sf, fillOpacity = 0.7, weight = 0.8, colour = "#e76254") 
}


# Import the shapefile ----------------------------------------------------

Peat_UAV_sf <- st_read("Data/Random_SHP/PeatUAVPotentialSites.shp") %>% 
  st_transform(., 4326) # transform to wgs84 to work with leaflet map


# Run it! -----------------------------------------------------------------

shp_viewer(Peat_UAV_sf, geom_number = 2)
