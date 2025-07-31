install.packages("sf")
library(sf)
library(dplyr)

# file path
path_data <- "C:/Users/alalor/OneDrive - DOI/_FireFX/Crew/maps/"

# Load CSV
df <- read.csv(paste0(path_data, "All Plots All Parks_2011.csv"))


# Fix inconsistent zone formats (e.g., '18S' marked as Northern hemisphere)
df <- df %>%
  mutate(UTM.Zone = ifelse(Hemisphere == "N" & grepl("S$", UTM.Zone),
                           sub("S$", "N", UTM.Zone),
                           UTM.Zone))

# Conversion function
convert_utm <- function(easting, northing, zone) {
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

# Apply conversion row-wise
df_coords <- bind_rows(lapply(1:nrow(df), function(i) {
  convert_utm(df$UTM.X[i], df$UTM.Y[i], df$UTM.Zone[i])
}))

# Final dataframe with lat/lon
df_final <- bind_cols(df, df_coords)

# Preview
head(df_final)

# save file
write.csv(df_final, paste0(path_data, "PlotCoordinates2.csv"), quote=FALSE, row.names = FALSE, na = "") 


_______________________________________________________________________
