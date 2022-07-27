
# GIS data sources through R
library(sf)
library(tidyverse)


# Country and admin boundaries --------------------------------------------

library(rnaturalearth)
#devtools::install_github("ropensci/rnaturalearthhires")

UK <- ne_countries(country = 'united kingdom', scale='large', returnclass='sf') %>%
  st_transform(crs = 27700) %>% 
  select(name, geometry)

England <- ne_states(geounit = 'england', returnclass='sf') %>%
  st_transform(crs = 27700) %>% 
  select(name, geometry)

plot(UK)

# Elevation data ----------------------------------------------------------

library(elevatr)

UK_DEM <- get_elev_raster(UK, z = 3, clip = "location", neg_to_na = "TRUE")

terra::plot(UK_DEM)

# Open street map data ----------------------------------------------------

library(osmdata)
 # find this can be glitchy!

AOI <- tibble(geom = "POINT(-3.537129 50.718405)") %>% 
  st_as_sf(., wkt = "geom", crs = 4326) %>% 
  st_transform(., crs = 27700) %>% 
  st_buffer(., 1000) %>% 
  st_transform(., crs = 4326)

get_rivers <- function(AOI_shp){
  osm_water <- opq(bbox = st_bbox(AOI_shp)) %>% 
    add_osm_feature(key = 'natural', value = 'water') %>% # Limit query to waterbodies
    osmdata_sf() # Convert to simple features 
  
  rivers <- osm_water$osm_multipolygons
  
  trim_rivers <- st_intersection(AOI_shp, rivers) %>% 
    st_union()
  
}

rivers <- get_rivers(AOI)


get_buildings <- function(AOI_shp){
  
  buildings <- opq(bbox = st_bbox(AOI_shp)) %>% 
    add_osm_feature(key = 'building') %>% # 
    osmdata_sf() 
  
  buildings_poly <- buildings$osm_polygons
  
  trim_builds <- st_intersection(AOI_shp, buildings_poly) %>% 
    st_union()
}

buildings <- get_buildings(AOI)

ggplot()+
  geom_sf(data = AOI, 
          colour = "black",
          fill = NA,
          size = 1)+
  geom_sf(data = buildings,
          fill = "gray20",
          colour = NA)+
  geom_sf(data = river,
          fill = "#0571b0",
          colour = "#0571b0")+
  theme_void()

# Viewing data on basemaps ------------------------------------------------

library(leaflet)

shp_viewer <- function(the_sf, geom_number = 1){
  
  centres <- st_coordinates(st_centroid(the_sf))
  
  leaflet() %>%  
    setView(lng = centres[geom_number,1], lat = centres[geom_number,2], zoom = 12) %>% 
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
    addProviderTiles(providers$Esri.WorldImagery,
                     options = providerTileOptions(opacity = 0.7)) %>% 
    addPolygons(data = the_sf, fillOpacity = 0.7, weight = 0.8) 
}


shp_viewer(AOI)

