
# Title:      Who does it wetter: Dartmoor or Bodmin 
# Objective:  Determine if Dartmoor or Bodmin was net wetter in 2019...
# Created by: Pia Benaud
# Created on: 08/02/2022


# Load packages -----------------------------------------------------------

library(ncdf4) # dealing with the netcdf data
library(sf) # for dealing with the shapefiles
library(terra) # for dealing with the raster data
library(dplyr) # for any wranglings
library(tidyr) # for pivot wider
library(lubridate) # working with dates and time
library(ggplot2) # plotting things up
library(ggdist) # for the fun plots!
library(MetBrewer) # colour pals
library(cowplot) # joining plots and legend func

#rm(list = ls()) # when you need a quick clean of the environment - DOES NOT UNLOAD PACKAGES!


# Import the data ---------------------------------------------------------

UK_AONB_sf <- st_read("Data/NE_AreasOfOutstandingNaturalBeautyEngland_SHP_Full/data/Areas_of_Outstanding_Natural_Beauty_England.shp")

Dartmoor_sf <- st_read("Data/NE_NationalParksEngland_SHP/data/National_Parks_England.shp")

Rainfall_2019_nc <- nc_open("Data/CEH_GEAR-monthly/CEH_GEAR_monthly_GB_2019.nc", auto_GMT = TRUE)


# Make a rainfall dataframe -----------------------------------------------

 # quick check of the time units
(ncatt_get(Rainfall_2019_nc, "time" , "units"))


rain_df <- function(ncdf4_file) {
  
  # extract the data
  east <- ncvar_get(ncdf4_file, "x" )
  north <- ncvar_get(ncdf4_file, "y" )
  time <- ncvar_get(ncdf4_file, "time" )
  rainfall <- ncvar_get(ncdf4_file, "rainfall_amount")
  
  # convert to useful time
  date <- ymd("1800-1-1") + days(time) # time is recorded as days since 1-1-1800 *check before running!
  
  # make the dataframe
  rainfall_data <- as.matrix(rainfall, ncol = 1)
  east_north_date <- expand.grid(east, north, date) 
  rainfall_df <- cbind(east_north_date, rainfall_data)
  names(rainfall_df) <- c("Easting", "Northing", "Month", "Precipitation")
  
  return(rainfall_df)
}
 

Rainfall_2019 <- rain_df(Rainfall_2019_nc) 


# Calculate annual total per 1 km -----------------------------------------

Total_Rainfall_2019 <- Rainfall_2019 %>% 
  group_by(Easting, Northing) %>% 
  summarise(Total_Rainfall = sum(Precipitation)) %>% 
  ungroup()
  
# now as a raster
Total_Rain_Rast <- rast(Total_Rainfall_2019, type="xyz")

rm(Rainfall_2019_nc, Rainfall_2019) # tidy

# Pull out the bodmin polygon ---------------------------------------------

poly_from_point <- function(sf_poly, point_ref){
  
  split_sf <- st_cast(UK_AONB_sf, "POLYGON", warn = FALSE)
  
  point_sf <- st_point(x = point_ref, dim = xy) %>% 
    st_sfc(.) %>% 
    st_set_crs(27700)
  
  the_sf <- split_sf %>% 
    mutate(ID = row_number()) %>%
    filter(ID %in% st_intersects(point_sf, .)) 
  
}


Bodmin_sf <- poly_from_point(UK_AONB_sf, point_ref = c(214040, 77925)) %>% 
  mutate(name = "Bodmin_AONB")

rm(UK_AONB_sf)

# Trim rasters to AOIs for plotting ---------------------------------------

raster_tidy_trim_df <- function(the_raster, the_sf){
  DF_out <- terra::mask(the_raster, vect(the_sf)) %>% 
    terra::crop(., the_sf) %>% 
    as.data.frame(., xy = TRUE) 
}

Bodmin_Rain <- raster_tidy_trim_df(Total_Rain_Rast, Bodmin_sf)

Dartmoor_Rain <- raster_tidy_trim_df(Total_Rain_Rast, Dartmoor_sf) %>% 
  mutate(Location = "Dartmoor")


# Make the fun plots ------------------------------------------------------

rain_plot <- function(rain_df){
  rain_df %>%
    ggplot(aes(x = Total_Rainfall, fill = Total_Rainfall, colour = Total_Rainfall)) +
    theme_classic()+
    labs(x='Total Rainfall (mm)') +
    scale_fill_gradientn(colours = met.brewer("Hiroshige"), limits = c(1000, 3200), name = "Total Rainfall (mm)") +
    scale_colour_gradientn(colours = met.brewer("Hiroshige"), limits = c(1000, 3200), guide ="none") +
    scale_x_continuous(limits = c(1000, 3200))+
    theme(line = element_blank(),
          text = element_blank(),
          title = element_blank(),
          legend.position = "none")+
    ggdist::stat_halfeye(adjust = 0.5, .width = 0, point_colour = 'NA', 
                         slab_colour='black', slab_size=0.4, slab_fill = "gray80") +
    ggdist::geom_dots(side = "bottom")
}

Bod_rain_cloud <- rain_plot(Bodmin_Rain)
Dart_rain_cloud <- rain_plot(Dartmoor_Rain)

# We should have a legend -------------------------------------------------

legend_fun <- function(.plot) { 
  the_legend <- cowplot::get_legend(
    .plot +
      guides(fill = guide_colourbar(ticks = FALSE, title.position = "left", title.vjust = 0.9))+
      theme(legend.direction = "horizontal",
            legend.position = "bottom",
            legend.margin = margin(t = 0, unit = "cm"),
            legend.key.width = unit(1, "cm"),
            legend.key.height = unit(0.3, "cm"),
            legend.title = element_text(size = 6, vjust = 1),
            legend.text = element_text(size = 4)))
  return(the_legend)
}

leg <- legend_fun(Dart_rain_cloud)


# Build the figure --------------------------------------------------------

(Who_does_it_wetter <- plot_grid(Dart_rain_cloud, Bod_rain_cloud, leg, nrow = 3, rel_heights = c(1,1,0.2), axis = "r",
                                 labels = c('Dartmoor', 'Bodmin', NA)))

png("Plots/who_does_it_wetter.png", width = 12, height = 12, units = 'cm', res = 280)
Who_does_it_wetter
dev.off()


# Make the raster plots ---------------------------------------------------

raster_plot <- function(the_sf, the_raster){
  ggplot() +
    geom_raster(data =  the_raster, 
                aes(x = x, y = y, fill = Total_Rainfall)) +
    geom_sf(data = the_sf, colour = "black", fill = NA, size = 1) +
    scale_fill_gradientn(colours = met.brewer("Hiroshige"), limits = c(1000, 3150), name = "Total Rainfall (mm)") +
    theme_classic() +
    theme(legend.position="none") +
    theme(line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
}

Bod_rain_rast <- raster_plot(Bodmin_sf, Bodmin_Rain)
Dart_rain_rast <- raster_plot(Dartmoor_sf, Dartmoor_Rain)


