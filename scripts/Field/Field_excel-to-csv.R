# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2025-07-30
#
# To take data from excel files and save individual protocols/tabs as CSVs,
# and name them appropriately

# Folder Setup:
# Navigate to where you store FX excel data files (e.g. Data/PARK/YEAR/Collected/)
# Put all your completed data collection excel spreadsheets into the "Collected" folder.
# Within this folder, create a folder called "CSV_Import to FFI"
# The "CSV_Import to FFI" folder should be empty. This is where CSVs will be stored after running this code.


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

# identify working directory (specifically user name)
here()

# select target park
target_park <- "TEST"

# load in data and name them based on file path
# change file path based on user name!
my_path_file <- "C:/Users/alalor/OneDrive - DOI/NER/FireFX/Data Collection/"
my_path_data <- paste0(my_path_file, target_park, "/Collected/")
my_path_csv <- paste0(my_path_file, target_park, "/Collected/CSV_Import to FFI/")


################################################################################
# CREATE LIST OF DATA NEEDED
################################################################################

# create list of file names
file_names_list <- list.files(my_path_data)

# specify file path each excel sheet
file_path <- paste0(my_path_data, file_names_list)

# add file paths and names to a dataframe. Filter by only excel files
file_names_df <- data.frame(FilePath = file_path, text = file_names_list) %>%
  filter(grepl(".xlsx", text)) %>%
  separate(text, sep = ".xlsx", into = ("FileName"))



################################################################################
# MAIN CODE / DO THE THING!
################################################################################

for (i in 1:nrow(file_names_df)) {
  
  path <- file_names_df[i, 1]
  name <- file_names_df[i, 2]
  
  # get list of sheet names in this Excel file
  sheets <- excel_sheets(path)
  
  # safe read function that checks for sheet presence
  safe_read_excel <- function(path, sheet) {
    if (sheet %in% sheets) {
      read_excel(path, sheet = sheet)
    } else {
      message(paste("Skipping missing sheet:", sheet, "in", basename(path)))
      return(NULL)
    }
  }
  
  # read only if the sheet exists
  Fuels1000     <- safe_read_excel(path, "Fuels CWD")
  FuelsDuffLitt <- safe_read_excel(path, "Fuels Duff-Litt")
  FuelsFine     <- safe_read_excel(path, "Fuels FWD")
  HerbsPoints   <- safe_read_excel(path, "Herbs (Points)")
  HerbsSpComp   <- safe_read_excel(path, "Herbs-Ob (Sp Comp)")
  Shrubs        <- safe_read_excel(path, "Shrubs (Belt)")
  Seedlings     <- safe_read_excel(path, "Seedlings (Quad)")
  Trees         <- safe_read_excel(path, "Trees")
  PostBurn      <- safe_read_excel(path, "Post Burn")
  
  # create csv paths
  my_path_csv_Fuels1000     <- paste0(my_path_csv, name, "_Fuels-CWD.csv")
  my_path_csv_FuelsDuffLitt <- paste0(my_path_csv, name, "_Fuels-DuffLitt.csv")
  my_path_csv_FuelsFine     <- paste0(my_path_csv, name, "_Fuels-FWD.csv")
  my_path_csv_HerbsPoints   <- paste0(my_path_csv, name, "_Herbs-CoverPoints.csv")
  my_path_csv_HerbsSpComp   <- paste0(my_path_csv, name, "_Herbs-CoverSpComp.csv")
  my_path_csv_Shrubs        <- paste0(my_path_csv, name, "_Shrubs-DensityBelts.csv")
  my_path_csv_Seedlings     <- paste0(my_path_csv, name, "_Seedlings-DensityQuads.csv")
  my_path_csv_Trees         <- paste0(my_path_csv, name, "_Trees.csv")
  my_path_csv_PostBurn      <- paste0(my_path_csv, name, "_PostBurn.csv")
  
  
  ############################################
  ### Herbs Points protocol reorganization ###
  ############################################

  # Herbs Points reorganization (only if present)
  if (!is.null(HerbsPoints)) {
    # reorganize HerbsPoints for additional species hits
    HerbsPoints_add <- HerbsPoints %>%
      # creates 8 new rows for additional species, replicating all info
      # creates new columns with unique species and GUIDs
      pivot_longer(cols = matches("^[2-8]spp|^[2-8]spp_GUID"),
                   names_to = c("extra", ".value"),
                   names_pattern = "([2-8])(.+)") %>%
      # filter out blank species entries
      filter(!is.na(spp)) %>%
      # replace replicated data with correct data (new species, GUID, status, order, and height)
      mutate(Species = spp,
             Status = "L",
             Spp_GUID = spp_GUID,
             Order = Order + as.integer(extra) - 1,
             Height = NA) %>%
      # remove extra columns
      select(!c(extra, spp, spp_GUID))
    # Combine original rows + new rows
    HerbsPoints <- bind_rows(HerbsPoints, HerbsPoints_add) %>%
      mutate(Species = toupper(Species)) %>%
      select(Index, Transect, Point, Tape, Order, Height, Species, Status, Comment, UV1, UV2, UV3, Spp_GUID) %>% 
      arrange(Transect, Point, Order)
    
    # Count Herb heights to make sure some data is present
    HerbsPointsCount <- sum(!is.na(HerbsPoints$Height))
    # identify substrate codes
    substrate <- c("BOLE", "DUFF", "HAY", "LITT", "LITTER", "MOSS", "ROCK", "SOIL", "WOOD")
  }

  
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
        distinct(Species, Spp_GUID),
      if (!is.null(Trees)) Trees %>% 
        mutate(Species = toupper(Species)) %>% 
        filter(!Species %in% substrate) %>%
        distinct(Species, Spp_GUID)
    ) %>% 
      filter(!is.na(Species)) %>% 
      distinct(Species, Spp_GUID)
    
    # identify species found in other protocols but not in HerbsSpComp
    UniqueSpecies_all_missing <- anti_join(UniqueSpecies_all, UniqueSpecies_HerbsSpComp)
    # merge all species with HerbsSpComp
    HerbsSpComp <- merge(HerbsSpComp, UniqueSpecies_all_missing, by = c("Species", "Spp_GUID"), all = T) %>% 
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
  
  #########################
  ### All protocol QAQC ###
  #########################
  
  # Delete empty rows, Change numbers in index column into ascending order
  if (!is.null(Fuels1000)) {
    Fuels1000 <- subset(Fuels1000, Dia != "") %>%
    mutate(Index = row_number())
  }
  if (!is.null(FuelsDuffLitt)) {
  FuelsDuffLitt <- subset(FuelsDuffLitt, LittDep != "") %>%
    mutate(Index = row_number())
  }
  if (!is.null(FuelsFine)) {
  FuelsFine <- subset(FuelsFine, OneHr != "") %>%
    mutate(Index = row_number()) %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  }
  if (!is.null(HerbsPoints)) {
  HerbsPoints <-
    mutate(HerbsPoints, Count = HerbsPointsCount) %>%
    subset(Count != "0") %>%
    select(!Count) %>% 
    mutate(Index = row_number()) %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  }
  if (!is.null(HerbsSpComp)) {
  HerbsSpComp <- subset(HerbsSpComp, !`Seen?` %in% c("N", "n")) %>%
    mutate(Index = row_number()) %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  }
  if (!is.null(Seedlings)) {
  Seedlings <- subset(Seedlings, Species != "") %>%
    mutate(Index = row_number()) %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  }
  if (!is.null(Shrubs)) {
  Shrubs <- subset(Shrubs, Species != "") %>%
    mutate(Index = row_number()) %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  }
  if (!is.null(Trees)) {
  Trees <- subset(Trees, Status != "X") %>%
    arrange(SubFrac, QTR, TagNo) %>%
    mutate(Index = row_number()) %>%
    mutate(IsVerified = "TRUE") %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  }
  if (!is.null(PostBurn)) {
  PostBurn <- subset(PostBurn, Sub != "") %>%
    mutate(Index = row_number())
  }
  
  
  # Helper function for safe write
  save_csv <- function(df, path, label) {
    if (is.null(df) || nrow(df) == 0) {
      message(paste(name, label, "is empty or missing"))
    } else {
      write.csv(df, path, quote = FALSE, row.names = FALSE, na = "")
    }
  }
  
  # Save each non-null sheet
  save_csv(Fuels1000, my_path_csv_Fuels1000, "Fuels CWD")
  save_csv(FuelsDuffLitt, my_path_csv_FuelsDuffLitt, "Fuels Duff-Litt")
  save_csv(FuelsFine, my_path_csv_FuelsFine, "Fuels FWD")
  save_csv(HerbsPoints, my_path_csv_HerbsPoints, "Herbs Points")
  save_csv(HerbsSpComp, my_path_csv_HerbsSpComp, "Herbs SpComp")
  save_csv(Shrubs, my_path_csv_Shrubs, "Shrubs")
  save_csv(Seedlings, my_path_csv_Seedlings, "Seedlings")
  save_csv(Trees, my_path_csv_Trees, "Trees")
  save_csv(PostBurn, my_path_csv_PostBurn, "Post Burn")
  
  message(paste("Finished:", name))
}

