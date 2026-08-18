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




############################################
### Herbs SpComp protocol reorganization ###
############################################

# Herbs SpComp reorganization (only if present)
if (!is.null(HerbsSpComp)) {
  UniqueSpecies_HerbsSpComp <- HerbsSpComp %>%
    mutate(Species = toupper(Species)) %>%
    distinct(Species, Spp_GUID)
  
  # build combined species list safely
  UniqueSpecies_all <- bind_rows(
    if (!is.null(HerbsPoints)) HerbsPoints %>% 
      mutate(Species = toupper(Species)) %>% 
      filter(!Species %in% substrate) %>%
      distinct(Species, Spp_GUID),
    if (!is.null(Seedlings)) Seedlings %>% 
      mutate(Species = toupper(Species)) %>% 
      filter(!Species %in% substrate) %>%
      distinct(Species, Spp_GUID)
  ) %>% 
    filter(!is.na(Species)) %>% 
    distinct(Species, Spp_GUID)
  
  # identify species found in other protocols but not in HerbsSpComp
  suppressMessages(UniqueSpecies_all_missing <- anti_join(UniqueSpecies_all, UniqueSpecies_HerbsSpComp))
  # merge all species with HerbsSpComp
  HerbsSpComp_test <- merge(HerbsSpComp, UniqueSpecies_all_missing, by = c("Species", "Spp_GUID"), all = T) %>% 
    mutate(Species = toupper(Species),
           Status = "L",
           `Seen?` = ifelse(is.na(`Seen?`), "Y", `Seen?`)) %>% 
    relocate(Species, .before = SizeCl) %>% 
    relocate(Status, .after = Species) %>% 
    relocate(Spp_GUID, .after = UV3) %>% 
    arrange(Species)
  
  # # identify if this version of HerbsSpComp is the primary version
  # HerbsSpComp <- HerbsSpComp %>% 
  #   mutate(Primary = ifelse(("N" %in% HerbsSpComp$`Seen?`) == TRUE, FALSE, TRUE))
}
