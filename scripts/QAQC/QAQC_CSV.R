# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2025-08-08
#
# To make sure FFI Exports appear as expected, to use them moving forward

################################################################################
# BEFORE STARTING
################################################################################

## Install packages (if needed)

# install.packages("here")
# install.packages("tidyverse")


## Load Packages

# "here" helps you easily find and reference your working directory
library(here)
# "tidyverse" has lots of useful functions for data cleaning
library(tidyverse)


################################################################################
# MAKE SURE FILE PATHS ARE CORRECT
################################################################################

# Identify working directory
here()

# designate target park(s)
# options: S-MTTS. S-Allegheny, N-MidAtlantic, NCA
target_zone <- "S-MTTS"
target_park <- "RICH"

# Load in data.
path_file <- "C:/Users/alalor/OneDrive - DOI/NER/FireFX/FFI Data Management/Exports/_metadata/"
path_data <- paste0(path_file, target_zone, "_", target_park, "/")
path_output <- paste0(here(), "/output/")


################################################################################
# LOAD DATA
################################################################################

MacroplotReport <- read.csv(paste0(path_data, target_park, "_", "MacroplotReport.csv"))
SampleEventReport <- read.csv(paste0(path_data, target_park, "_", "SampleEventReport.csv"))
ProjectUnitReport <- read.csv(paste0(path_data, target_park, "_", "ProjectUnitAssignmentReport.csv"))


################################################################################
# SEPARATE DATA
################################################################################

#The Macroplot Report contains information on both plot metadata, and sample event history. 
#Separate these into 2 tables. These will be formatted separately and then merged back together.

MacroplotReport_MetaData <- MacroplotReport %>%
  mutate(SampleEventDate = ifelse(SampleEventDate == "", NA, SampleEventDate)) %>% 
  filter(is.na(SampleEventDate)) %>%
  select(!c(SampleEventDate, SampleEventTeam, SampleEventComment, MonStatusOrd, LegacyMonStatus, MonStatus))

MacroplotReport_SampleEvents <- MacroplotReport %>%
  mutate(SampleEventDate = ifelse(SampleEventDate == "", NA, SampleEventDate)) %>% 
  filter(!is.na(SampleEventDate)) %>%
  select(c(Macroplot, SampleEventDate, SampleEventTeam, SampleEventComment, MonStatusOrd, LegacyMonStatus, MonStatus))


################################################################################
# CHECK DATA
################################################################################

# Macroplot Report - Meta Data
str(MacroplotReport_MetaData)
unique(MacroplotReport_MetaData$Macroplot)
unique(MacroplotReport_MetaData$Purpose)
#unique(MacroplotReport_MetaData$LocatedBy)
unique(MacroplotReport_MetaData$Type)
unique(MacroplotReport_MetaData$Latitude)
unique(MacroplotReport_MetaData$Longitude)
unique(MacroplotReport_MetaData$UTM_X)
unique(MacroplotReport_MetaData$UTM_Y)
unique(MacroplotReport_MetaData$UTM_Zone)
unique(MacroplotReport_MetaData$Datum)
unique(MacroplotReport_MetaData$PDOP)
unique(MacroplotReport_MetaData$Precision)
unique(MacroplotReport_MetaData$DateIn)
unique(MacroplotReport_MetaData$FutureVisit)
unique(MacroplotReport_MetaData$DateOut)
unique(MacroplotReport_MetaData$Elevation)
unique(MacroplotReport_MetaData$ElevationUnits)
unique(MacroplotReport_MetaData$Azimuth)
unique(MacroplotReport_MetaData$Aspect)
unique(MacroplotReport_MetaData$SlopeHill)
unique(MacroplotReport_MetaData$SlopeTransect)
#unique(MacroplotReport_MetaData$StartPoint)
#unique(MacroplotReport_MetaData$Directions)
#unique(MacroplotReport_MetaData$Comments)
#unique(MacroplotReport_MetaData$MetaData)
#unique(MacroplotReport_MetaData$UV1)
#unique(MacroplotReport_MetaData$UV2)
#unique(MacroplotReport_MetaData$UV3)
#unique(MacroplotReport_MetaData$UV4)
#unique(MacroplotReport_MetaData$UV5)
#unique(MacroplotReport_MetaData$UV6)
#unique(MacroplotReport_MetaData$UV7)
#unique(MacroplotReport_MetaData$UV8)

# Macroplot Report - Sample Events
str(MacroplotReport_SampleEvents)
unique(MacroplotReport_SampleEvents$Macroplot)
unique(MacroplotReport_SampleEvents$SampleEventDate)
#unique(MacroplotReport_SampleEvents$SampleEventTeam)
#unique(MacroplotReport_SampleEvents$SampleEventComment)
unique(MacroplotReport_SampleEvents$MonStatusOrd)
unique(MacroplotReport_SampleEvents$LegacyMonStatus)
unique(MacroplotReport_SampleEvents$MonStatus)

# Sample Event Report

# Project Unit Report















