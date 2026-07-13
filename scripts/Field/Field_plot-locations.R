# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2025-11-05
#
# To create a file with consolidated plot locations
# 
# Before using this script you must create a MetadataReport.csv file for each park.
# You can create this file by using the script "Clean+Merge_FFI.Rmd" in the 1_Clean folder


# load packages
library(sf)
library(dplyr)
library(purrr)
library(here)


# load in data
path_data <- "Z:/NER/SHEN/RAD/Fire/Fire Mgmt/FIRE_ECOLOGIST_FILES/FFI_DATA_MANAGEMENT/Exports_clean/FFI/"
path_output <- paste0(here(), "/output/data_clean/")
parks <- c("ANTI", "APCO", "CATO", "CUVA", "DEWA", "FRSP", "GATE", "GETT", "MONO", "NERI", "PRWI", "RICH", "SARA", "SHEN", "VAFO")



for(i in parks) {
  # select target park
  target_park <- i
  
  # load CSV
  #path_data <- paste0(path_file, target_park, "/", target_park, "_")
  data_raw <- read.csv(paste0(path_data, "all_metadata.csv")) %>% 
    filter(AdminUnit_Name == target_park)
  
  # group
  data_locations <- data_raw %>%
    select(c(AdminUnit_Name, Macroplot_Purpose, Macroplot_Type, Macroplot_Name, ProjectUnit_Name, Macroplot_UV1, Macroplot_UV2, 
             Macroplot_Lat_dd, Macroplot_Long_dd, Macroplot_UtmX_m, Macroplot_UtmY_m, Macroplot_UtmZone, Macroplot_Datum)) %>%
    distinct()
  
  # filter
  data_final <- data_locations %>%
    filter(!is.na(Macroplot_Lat_dd),
           !is.na(Macroplot_Long_dd))
  
  # save file
  write.csv(data_final, paste0(path_output, target_park, "_PlotLocations.csv"), quote=FALSE, row.names = FALSE)
}



## All parks, all FMH plots

  # load CSV
  data_raw <- read.csv(paste0(path_data, "all_metadata.csv")) %>% 
    filter(Macroplot_Purpose != "Composite Burn Index") %>% 
    filter(Macroplot_Purpose != "Inventory & Monitoring")
  
  # group
  data_locations <- data_raw %>%
  select(c(AdminUnit_Name, Macroplot_Purpose, Macroplot_Type, Macroplot_Name, ProjectUnit_Name, Macroplot_UV1, Macroplot_UV2, 
           Macroplot_Lat_dd, Macroplot_Long_dd, Macroplot_UtmX_m, Macroplot_UtmY_m, Macroplot_UtmZone, Macroplot_Datum)) %>%
  distinct()
    
  # filter
  data_final <- data_locations %>%
    filter(!is.na(Macroplot_Lat_dd),
           !is.na(Macroplot_Long_dd))
    
  # save file
  write.csv(data_final, paste0(path_output, "_NER_FMH_PlotLocations.csv"), quote=FALSE, row.names = FALSE)

    
    
    
    
## All parks, all plots, even if they don't have locations
  
  # load CSV
  data_raw <- read.csv(paste0(path_data, "all_metadata.csv"))
  
  # group
  data_locations <- data_raw %>%
    select(c(AdminUnit_Name, Macroplot_Purpose, Macroplot_Type, Macroplot_Name, ProjectUnit_Name, Macroplot_UV1, Macroplot_UV2, 
             Macroplot_Lat_dd, Macroplot_Long_dd, Macroplot_UtmX_m, Macroplot_UtmY_m, Macroplot_UtmZone, Macroplot_Datum)) %>%
    distinct()
  
  # save file
  write.csv(data_locations, paste0(path_output, "_NER_ALL_PlotLocations.csv"), quote=FALSE, row.names = FALSE)
  
  
  
  
  

