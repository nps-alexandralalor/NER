# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2025-11-05
#
# To create a file with consolidated plot locations

# install packages
install.packages("sf")

# load packages
library(sf)
library(dplyr)
library(purrr)
library(here)

# designate target park(s)
target_park <- "SHEN"

# load in data.
path_file <- "C:/Users/alalor/OneDrive - DOI/NER/FireFX/FFI Data Management/Exports/"
path_data <- paste0(path_file, target_park, "/")
path_output <- paste0(here(), "/output/data_clean/")
path_output <- "C:/Users/alalor/OneDrive - DOI/R/NER/output/data_clean/"

# load CSV
df_raw <- read.csv(paste0(path_data, target_park, "_MetadataReport.csv"), quote = "")

# group
df_locations <- df_raw %>% 
  select(!c(SampleEventTeam, SampleEventComment, LegacyMonStatus, MonStatus, Protocols, Visited, SampleEventDate)) %>% 
  distinct() %>% 
  select(c(AdministrationUnit_Name, Purpose, Type, Macroplot, ProjectUnit, Latitude, Longitude, UTM_X, UTM_Y, UTM_Zone, Datum, PDOP, Precision))

# filter
df_locations <- df_locations %>% 
  filter(!is.na(Latitude),
         !is.na(Longitude))

# save file
write.csv(df_final, paste0(path_output, target_park, "_PlotLocations.csv"), quote=FALSE, row.names = FALSE, na = "") 


