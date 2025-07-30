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

database <- "RICH"
MonitoringType <- "Malvern Hill"
plot <- "01"


################################################################################
# LOAD DATA
################################################################################

# Load in data
Species <- read.csv(paste0(path_data, "RICH species list 2025_raw.csv"))
HerbsObs <- read.csv(paste0(path_data, "RICH_MalvernHill_Cover - Species Composition (metric)_XPT.csv"), quote = "")
HerbsPoints <- read.csv(paste0(path_data, "RICH_MalvernHill_Cover - Points (metric)_XPT.csv"), quote = "")


################################################################################
# MERGE PROTOCOLS
################################################################################

#identify all species in herbs protocols
Herbs <- merge(HerbsPoints, HerbsObs, by = "Species.Symbol", all = T) %>% 
  mutate(LocalSpecies_Symbol = Species.Symbol) %>% 
  select(LocalSpecies_Symbol) %>% 
  mutate(MalvernHill = "yes")


################################################################################
# MERGE WITH MASTER LIST
################################################################################

#identify all species in herbs protocols
Combined <- merge(Species, Herbs, by = "LocalSpecies_Symbol", all = T) %>% 
  mutate(MalvernHill = ifelse(is.na(MalvernHill), "no", MalvernHill))


################################################################################
# SAVE
################################################################################

write.csv(Combined, paste0(path_clean, "RICH_MalvernHill_Species_all.csv"), quote=FALSE, row.names = FALSE, na = "") 








