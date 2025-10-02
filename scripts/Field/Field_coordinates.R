install.packages("sf")
library(sf)
library(dplyr)
library(here)

# file path

# designate target park(s)
target_park <- "SHEN"

# Load in data.
path_file <- "C:/Users/alalor/OneDrive - DOI/NER/FireFX/FFI Data Management/Exports/"
path_data <- paste0(path_file, target_park, "/")
path_output <- paste0(here(), "/output/data_clean/")

# Load CSV
df_raw <- read.csv(paste0(path_data, target_park, "_MetadataReport.csv"), quote = "")

# Group
df_locations <- df_raw %>% 
  select(!c(SampleEventTeam, SampleEventComment, LegacyMonStatus, MonStatus, Protocols, Visited, SampleEventDate)) %>% 
  distinct() %>% 
  select(c(AdministrationUnit_Name, Purpose, Type, Macroplot, ProjectUnit, Latitude, Longitude, UTM_X, UTM_Y, UTM_Zone, Datum, PDOP, Precision))

# Filter
df_locations_FMH <- df_locations %>% 
  filter(Type == "Forest",
         ProjectUnit != "GrayBirch (INACTIVE)")





# Fix inconsistent zone formats (e.g., '18S' marked as Northern hemisphere)
df <- df_locations_FMH %>%
  mutate(UTM_Zone = ifelse(grepl("S$", UTM_Zone),
                           sub("S$", "", UTM_Zone),
                           UTM_Zone))
  select(!c(Latitude, Longitude))

# Conversion function
convert_utm_1 <- function(easting, northing, zone) {
  zone_num <- as.numeric(gsub("[^0-9]", "", zone))
  is_south <- grepl("S$", zone)
  
  epsg_code <- ifelse(is_south, 32700, 32600) + zone_num
  utm_sf <- st_as_sf(data.frame(x = easting, y = northing),
                     coords = c("x", "y"),
                     crs = paste0("EPSG:", epsg_code))
  latlon <- st_transform(utm_sf, crs = 4326)
  coords <- st_coordinates(latlon)
  return(data.frame(Latitude = coords[,2], Longitude = coords[,1]))
}

convert_utm <- function(easting, northing, zone) {
  zone_num <- as.numeric(zone)
  epsg_code <- 32600 + zone_num
  utm_sf <- st_as_sf(data.frame(x = easting, y = northing),
                     coords = c("x", "y"),
                     crs = paste0("EPSG:", epsg_code))
  latlon <- st_transform(utm_sf, crs = 4326)
  coords <- st_coordinates(latlon)
  return(data.frame(Latitude = coords[,2], Longitude = coords[,1]))
}

# Apply conversion row-wise
df_coords <- bind_rows(lapply(1:nrow(df), function(i) {
  convert_utm(df$UTM_X[i], df$UTM_Y[i], df$UTM_Zone[i])
}))




# Final dataframe with lat/lon
df_final <- bind_cols(df, df_coords)

# Preview
head(df_final)

# save file
write.csv(df_final, paste0(path_output, target_park, "_PlotCoordinates.csv"), quote=FALSE, row.names = FALSE, na = "") 


_______________________________________________________________________
