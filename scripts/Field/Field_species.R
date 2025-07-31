# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2025-07-30
#
# To take data from excel files and save individual protocols/tabs as CSVs,
# and name them appropriately


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

#load in data and name them based on file path
path_data <- "C:/Users/alalor/OneDrive - DOI/_FireFX/Data Collection/NER/RICH/Template/Species/"
path_clean <- paste0(here(), "/output/data_clean/")


################################################################################
# LOAD DATA
################################################################################

# Load in data
Species <- read.csv(paste0(path_data, "RICH_species.csv"))
MH_HerbsObs <- read.csv(paste0(path_data, "RICH_MalvernHill_Cover - Species Composition (metric)_XPT.csv"), quote = "")
MH_HerbsPoints <- read.csv(paste0(path_data, "RICH_MalvernHill_Cover - Points (metric)_XPT.csv"), quote = "")
CH_HerbsObs <- read.csv(paste0(path_data, "RICH_ColdHarbor_Cover - Species Composition (metric)_XPT.csv"), quote = "")
CH_HerbsPoints <- read.csv(paste0(path_data, "RICH_MalvernHill_Cover - Points (metric)_XPT.csv"), quote = "")


################################################################################
# CLEAN DATA
################################################################################

#ensure no commas exist
Species <- Species %>% 
  mutate(LocalSpecies_CommonName = gsub(',', ";", LocalSpecies_CommonName),
         LocalSpecies_LifeCycle = gsub(',', ";", LocalSpecies_LifeCycle),
         LocalSpecies_Comment = gsub(',', ";", LocalSpecies_Comment))


################################################################################
# MERGE PROTOCOLS
################################################################################

#identify all species in herbs protocols
MH_Herbs <- merge(MH_HerbsPoints, MH_HerbsObs, by = "Species.Symbol", all = T) %>% 
  filter(Species.Symbol != "") %>% 
  mutate(LocalSpecies_Symbol = Species.Symbol) %>% 
  select(LocalSpecies_Symbol) %>% 
  mutate(MalvernHill = "yes")

CH_Herbs <- merge(CH_HerbsPoints, CH_HerbsObs, by = "Species.Symbol", all = T) %>% 
  filter(Species.Symbol != "") %>% 
  mutate(LocalSpecies_Symbol = Species.Symbol) %>% 
  select(LocalSpecies_Symbol) %>% 
  mutate(ColdHarbor = "yes")


################################################################################
# MERGE WITH MASTER LIST
################################################################################

#ensure no commas exist
Species <- Species %>% 
  mutate(LocalSpecies_CommonName = gsub(',', ";", LocalSpecies_CommonName),
         LocalSpecies_LifeCycle = gsub(',', ";", LocalSpecies_LifeCycle),
         LocalSpecies_Comment = gsub(',', ";", LocalSpecies_Comment))

#identify all species in herbs protocols
Species_all_1 <- merge(Species, MH_Herbs, by = "LocalSpecies_Symbol", all = T) %>% 
  mutate(MalvernHill = ifelse(is.na(MalvernHill), "no", MalvernHill))

Species_all <- merge(Species_all_1, CH_Herbs, by = "LocalSpecies_Symbol", all = T) %>% 
  mutate(ColdHarbor = ifelse(is.na(ColdHarbor), "no", ColdHarbor))

################################################################################
# SAVE
################################################################################

write.csv(Species_all, paste0(path_clean, "RICH_species_all.csv"), quote=FALSE, row.names = FALSE, na = "") 








