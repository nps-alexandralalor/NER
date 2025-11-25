# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2025-11-05
#
# To convert UTM coordinates to Lat Long coordinates

# install packages
install.packages("sf")

# load packages
library(sf)
library(dplyr)
library(purrr)
library(here)

# designate target park(s)
target_park <- "SHEN"

# Load in data.
path_file <- "C:/Users/alalor/OneDrive - DOI/NER/FireFX/FFI Data Management/Exports/"
path_data <- paste0(path_file, target_park, "/")
path_output <- paste0(here(), "/output/data_clean/")
path_output <- "C:/Users/alalor/OneDrive - DOI/R/NER/output/data_clean/"

# Load CSV
df_raw <- read.csv(paste0(path_data, target_park, "_MetadataReport.csv"), quote = "")

# Group
df_locations <- df_raw %>% 
  select(!c(SampleEventTeam, SampleEventComment, LegacyMonStatus, MonStatus, Protocols, Visited, SampleEventDate)) %>% 
  distinct() %>% 
  select(c(AdministrationUnit_Name, Purpose, Type, Macroplot, ProjectUnit, UTM_X, UTM_Y, UTM_Zone, Datum))

# Filter
df_filtered <- df_locations %>% 
  filter(UTM_X != 0.0,
         !is.na(UTM_X))

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
  mutate(EPSG = map2_int(Datum, UTM_Zone, get_epsg))

# Convert to Lat Long using UTM_X, UTM_Y, UTM_Zone, and EPSG (converted Datum)
df_coords <- bind_rows(lapply(1:nrow(df), function(i) {
  convert_utm(df$UTM_X[i], df$UTM_Y[i], df$UTM_Zone[i], df$EPSG[i])
}))

# Final dataframe with lat/lon
df_final <- bind_cols(df, df_coords) %>% 
  select(!c(EPSG))

# save file
write.csv(df_final, paste0(path_output, target_park, "_PlotCoordinates.csv"), quote=FALSE, row.names = FALSE, na = "") 


