# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2025-07-30
#
# Combine species outputs for complete plot list


################################################################################
# BEFORE STARTING
################################################################################

#install packages
install.packages("tidyverse")
install.packages("here")
#load packages
library(tidyverse)
library(readxl)
library(dplyr)
library(here)


################################################################################
# MAKE SURE FILE PATHS ARE CORRECT
################################################################################

#identify working directory (specifically user name)
here()

# select target park
target_park <- "GETT"
target_plot <-"Plot 14"

# load in data and name them based on file path
# change file path based on user name!
path_file <- "C:/Users/alalor/OneDrive - DOI/NER/FireFX/Data Collection/"
path_data <- paste0(path_file, target_park, "/Template/CSV_Old Data/")


################################################################################
# LOAD DATA
################################################################################

# Load in data
HerbsPoints <- read.csv(paste0(path_data, target_plot, "_Cover - Points (metric).csv"), skip = 2)
HerbsSpComp <- read.csv(paste0(path_data, target_plot, "_Cover - Species Composition (metric).csv"), skip = 2)


################################################################################
# CLEAN DATA
################################################################################

# change column name
HerbsSpComp <- HerbsSpComp %>% 
  mutate(Species = Item.Code) %>% 
  select(!Item.Code)


################################################################################
# MERGE SPECIES
################################################################################

# isolate unique species in all protocols
UniqueSpecies_HerbsPoints <- HerbsPoints %>% 
  filter(Status == "L") %>% 
  distinct(Species)
UniqueSpecies_HerbsSpComp <- HerbsSpComp %>% 
  distinct(Species)

# identify species found in HerbsPoints but not in HerbsSpComp
UniqueSpecies_missing <- anti_join(UniqueSpecies_HerbsPoints, UniqueSpecies_HerbsSpComp)

# merge all species with HerbsSpComp
HerbsSpComp_new <- merge(HerbsSpComp, UniqueSpecies_missing, by = c("Species"), all = T) %>% 
  select(Species) %>% 
  arrange(Species)


################################################################################
# SAVE
################################################################################

write.csv(HerbsSpComp_new, paste0(path_data, target_plot, "_CoverSpComp.csv"), quote=FALSE, row.names = FALSE, na = "") 


