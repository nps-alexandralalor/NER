# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2025-11-05
#
# To convert UTM coordinates to Lat Long coordinates

# install packages
#install.packages("sf")

# load packages
library(sf)
library(dplyr)
library(purrr)
library(here)

# designate target park(s)
target_park <- "NERI"

# Adjust filr paths
path_data <- "Z:/NER/SHEN/RAD/Fire/Fire Mgmt/FIRE_ECOLOGIST_FILES/FFI_DATA_MANAGEMENT/Exports_clean/FFI/"
path_output <- paste0(here(), "/output/data_clean/")

# Load and filter data
data_raw <- read.csv(paste0(path_data, "all_metadata.csv")) %>% 
  filter(AdminUnit_Name == target_park)

# Group
df_locations <- data_raw %>% 
  select(!c(SampleEvent_Who, SampleEvent_Comment, SampleEvent_Date, SampleEvent_LegacyMonitoringStatus, 
            SampleEvent_Protocols, SampleEvent_Visited, MonitoringStatus_Name)) %>% 
  distinct() %>% 
  select(c(AdminUnit_Name, Macroplot_Purpose, Macroplot_Type, Macroplot_Name, ProjectUnit_Name,
           Macroplot_UtmX_m, Macroplot_UtmY_m, Macroplot_UtmZone, Macroplot_Datum))

# Filter
df_filtered <- df_locations %>% 
  filter(Macroplot_UtmX_m != 0.0,
         !is.na(Macroplot_UtmX_m))


# write.csv(df_filtered, paste0(path_output, target_park, "_PlotCoordinates.csv"), quote=FALSE, row.names = FALSE, na = "") 
# df_filtered <- read.csv(paste0(here(), "/output/data_clean/", target_park, "_PlotCoordinates.csv"))

################
# Functions to convert coordinates
################

# Function to get EPSG based on Datum and Zone
get_epsg <- function(datum, zone) {
  if (toupper(datum) == "NAD27") {
    return(26700 + zone)   # NAD27 / UTM zone XXN
  } else if (toupper(datum) == "NAD83") {
    return(26900 + zone)   # NAD83 / UTM zone XXN
  } else if (toupper(datum) == "WGS84") {
    return(32600 + zone)   # WGS84 / UTM zone XXN
  } else {
    return(NA_integer_)
  }
}

# Conversion function
convert_utm <- function(easting, northing, zone, EPSG) {
  zone_num <- as.numeric(zone)
  utm_sf <- st_as_sf(data.frame(x = easting, y = northing),
                     coords = c("x", "y"),
                     crs = EPSG)
  latlon <- st_transform(utm_sf, crs = 4326)
  coords <- st_coordinates(latlon)
  return(data.frame(Latitude = coords[,2], Longitude = coords[,1]))
}


################
# Convert coordinates
################

# Add EPSG column to correctly convert different Datums
df <- df_filtered %>% 
  mutate(EPSG = map2_int(Macroplot_Datum, Macroplot_UtmZone, get_epsg))

# Convert to Lat Long using UTM_X, UTM_Y, UTM_Zone, and EPSG (converted Datum)
df_coords <- bind_rows(lapply(1:nrow(df), function(i) {
  convert_utm(df$Macroplot_UtmX_m[i], df$Macroplot_UtmY_m[i], df$Macroplot_UtmZone[i], df$EPSG[i])
}))

# Final dataframe with lat/lon
df_final <- bind_cols(df, df_coords) %>% 
  select(!c(EPSG))

# save file
write.csv(df_final, paste0(path_output, target_park, "_PlotCoordinates.csv"), quote=FALSE, row.names = FALSE, na = "") 


