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
target_park <- "GETT"

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
# PRACTICE CODE
################################################################################



################################################################################
# MAIN CODE / DO THE THING!
################################################################################

#separate excel files into tabs, save as CSVs, and name them appropriately
for(i in 1:nrow(file_names_df)) {
  
  #############
  ### Setup ###
  #############
  
  path <- file_names_df[i,1]
  name <- file_names_df[i,2]

  #read tabs of excel files, bring them into R
  Fuels1000 <- read_excel(path, sheet = "Fuels CWD")
  FuelsDuffLitt <- read_excel(path, sheet = "Fuels Duff-Litt")
  FuelsFine <- read_excel(path, sheet = "Fuels FWD")
  HerbsPoints <- read_excel(path, sheet = "Herbs (Points)")
  HerbsSpComp <- read_excel(path, sheet = "Herbs-Ob (Sp Comp)")
  Shrubs <- read_excel(path, sheet = "Shrubs (Belt)")
  Seedlings <- read_excel(path, sheet = "Seedlings (Quad)")
  Trees <- read_excel(path, sheet = "Trees")
  PostBurn <- read_excel(path, sheet = "Post Burn")

  #create csv paths
  my_path_csv_Fuels1000 <- paste0(my_path_csv, name, "_Fuels1000.csv")
  my_path_csv_FuelsDuffLitt <- paste0(my_path_csv, name, "_FuelsDuffLitt.csv")
  my_path_csv_FuelsFine <- paste0(my_path_csv, name, "_FuelsFine.csv")
  my_path_csv_HerbsPoints <- paste0(my_path_csv, name, "_HerbsPoints.csv")
  my_path_csv_HerbsSpComp<- paste0(my_path_csv, name, "_HerbsSpComp.csv")
  my_path_csv_Shrubs<- paste0(my_path_csv, name, "_Shrubs.csv")
  my_path_csv_Seedlings <- paste0(my_path_csv, name, "_Seedlings.csv")
  my_path_csv_Trees <- paste0(my_path_csv, name, "_Trees.csv")
  my_path_csv_PostBurn <- paste0(my_path_csv, name, "_PostBurn.csv")

  ############################################
  ### Herbs Points protocol reorganization ###
  ############################################
  
  # reorganize HerbsPoints for additional species hits
  HerbsPoints_add <- HerbsPoints %>%
    pivot_longer(cols = matches("^[2-8]spp|^[2-8]spp_GUID"),
                 names_to = c("extra", ".value"),
                 names_pattern = "([2-8])(.+)") %>%
    filter(!is.na(spp)) %>%     # only keep extra species that exist
    mutate(Species = spp,
           Status = "L",
           Spp_GUID = spp_GUID,
           Order = Order + as.integer(extra) - 1,
           Height = NA) %>%
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
  
  ############################################
  ### Herbs SpComp protocol reorganization ###
  ############################################
  
  # isolate unique species in all protocols
  UniqueSpecies_HerbsSpComp <- HerbsSpComp %>% 
    mutate(Species = toupper(Species)) %>% 
    distinct(Species, Spp_GUID)
  UniqueSpecies_HerbsPoints <- HerbsPoints %>% 
    mutate(Species = toupper(Species)) %>% 
    filter(!Species %in% substrate) %>%
    distinct(Species, Spp_GUID)
  UniqueSpecies_Seedlings <- Seedlings %>% 
    mutate(Species = toupper(Species)) %>% 
    filter(!Species %in% substrate) %>%
    distinct(Species, Spp_GUID)
  UniqueSpecies_Trees <- Trees %>% 
    mutate(Species = toupper(Species)) %>% 
    filter(!Species %in% substrate) %>%
    distinct(Species, Spp_GUID)
  # combine all species
  UniqueSpecies_all <- rbind(UniqueSpecies_HerbsPoints, UniqueSpecies_Seedlings, UniqueSpecies_Trees) %>% 
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
  
  
  #########################
  ### All protocol QAQC ###
  #########################

  # Delete empty rows, Change numbers in index column into ascending order
  Fuels1000 <- subset(Fuels1000, Dia != "") %>%
    mutate(Index = row_number())
  FuelsDuffLitt <- subset(FuelsDuffLitt, LittDep != "") %>%
    mutate(Index = row_number())
  FuelsFine <- subset(FuelsFine, OneHr != "") %>%
    mutate(Index = row_number()) %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  HerbsPoints <-
    mutate(HerbsPoints, Count = HerbsPointsCount) %>%
    subset(Count != "0") %>%
    select(!Count) %>% 
    mutate(Index = row_number()) %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  HerbsSpComp <- subset(HerbsSpComp, !`Seen?` %in% c("N", "n")) %>%
    mutate(Index = row_number()) %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  Seedlings <- subset(Seedlings, Species != "") %>%
    mutate(Index = row_number()) %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  Shrubs <- subset(Shrubs, Species != "") %>%
    mutate(Index = row_number()) %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  Trees <- subset(Trees, Status != "X") %>%
    arrange(SubFrac, QTR, TagNo) %>%
    mutate(Index = row_number()) %>%
    mutate(IsVerified = "TRUE") %>%
    map_df(str_replace_all, pattern = ",", replacement = ";")
  PostBurn <- subset(PostBurn, Sub != "") %>%
    mutate(Index = row_number())
  
  ##################
  ### Save files ###
  ##################
  
  #create CSVs, exclude blank data frames
  if(dim(Fuels1000)[1] == 0) {print(paste0(name," ","Fuels CWD is empty"))}
    else{write.csv(Fuels1000, my_path_csv_Fuels1000, quote=FALSE, row.names = FALSE, na = "")}
  if(dim(FuelsDuffLitt)[1] == 0) {print(paste0(name," ","Fuels Duff-Litt is empty"))}
     else{write.csv(FuelsDuffLitt, my_path_csv_FuelsDuffLitt, quote=FALSE, row.names = FALSE, na = "")}
  if(dim(FuelsFine)[1] == 0) {print(paste0(name," ","Fuels FWD is empty"))}
  else{write.csv(FuelsFine, my_path_csv_FuelsFine, quote=FALSE, row.names = FALSE, na = "")}
  if(dim(HerbsPoints)[1] == 0) {print(paste0(name," ","Herbs Points is empty"))}
     else{write.csv(HerbsPoints, my_path_csv_HerbsPoints, quote=FALSE, row.names = FALSE, na = "")}
  if(dim(HerbsSpComp)[1] == 0) {print(paste0(name," ","Herbs SpComp is empty"))}
     else{write.csv(HerbsSpComp, my_path_csv_HerbsSpComp, quote=FALSE, row.names = FALSE, na = "")}
  if(dim(Shrubs)[1] == 0) {print(paste0(name," ","Shrubs is empty"))}
     else{write.csv(Shrubs, my_path_csv_Shrubs, quote=FALSE, row.names = FALSE, na = "")}
  if(dim(Seedlings)[1] == 0) {print(paste0(name," ","Seedlings is empty"))}
     else{write.csv(Seedlings, my_path_csv_Seedlings, quote=FALSE, row.names = FALSE, na = "")}
  if(dim(Trees)[1] == 0) {print(paste0(name," ","Trees is empty"))}
    else{write.csv(Trees, my_path_csv_Trees, quote=FALSE, row.names = FALSE, na = "")}
  if(dim(PostBurn)[1] == 0) {print(paste0(name," ","Post Burn is empty"))}
    else{write.csv(PostBurn, my_path_csv_PostBurn, quote = FALSE, row.names = FALSE, na = "")}
  }


