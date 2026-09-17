# Created by: Alexandra Lalor
# Email: alexandra_lalor@nps.gov, allielalor@gmail.com
# Date Created: 2026-09-10
#
# Repository of monitoring objectives


################################################################################
# BEFORE STARTING
################################################################################

#install packages
#install.packages("tidyverse")

#load packages
library(tidyverse)


################################################################################
# TARGET VALUES
################################################################################

# Monitoring Types
#c("Oak Hickory", "Pine Oak", "Mixed Oak", "Gray Birch", "Modified Disturbed", "Xeric Oak", "Mesic Oak")

target_FineFuelLoading <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 1.4,
         ifelse(target_MonitoringType %in% "Xeric Oak", 2.4,
                ifelse(target_MonitoringType %in% "Mesic Oak", 2.4,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 2.4,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 2.4, NA)))))

target_Duff <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 1.0,
         ifelse(target_MonitoringType %in% "Xeric Oak", 1.0,
                ifelse(target_MonitoringType %in% "Mesic Oak", 1.5,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 1.0,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 1.0, NA)))))

target_Sapling <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 1000,
         ifelse(target_MonitoringType %in% "Xeric Oak", 1500,
                ifelse(target_MonitoringType %in% "Mesic Oak", 2000,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 1500,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 1500, NA)))))

target_PoleXericDens <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 100,
         ifelse(target_MonitoringType %in% "Xeric Oak", 100,
                ifelse(target_MonitoringType %in% "Mesic Oak", 100,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 100,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 100, NA)))))

target_PoleMesicDens <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 100,
         ifelse(target_MonitoringType %in% "Xeric Oak", 100,
                ifelse(target_MonitoringType %in% "Mesic Oak", 200,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 100,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 75, NA)))))

target_OverstoryXericRatio <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 80,
         ifelse(target_MonitoringType %in% "Xeric Oak", 80,
                ifelse(target_MonitoringType %in% "Mesic Oak", 30,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 80,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 80, NA)))))

target_OverstoryMesicRatio <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 20,
         ifelse(target_MonitoringType %in% "Xeric Oak", 20,
                ifelse(target_MonitoringType %in% "Mesic Oak", 70,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 20,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 20, NA)))))

target_StockingLow <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 30,
         ifelse(target_MonitoringType %in% "Xeric Oak", 40,
                ifelse(target_MonitoringType %in% "Mesic Oak", 50,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 40,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 40, NA)))))

target_StockingHigh <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 70,
         ifelse(target_MonitoringType %in% "Xeric Oak", 70,
                ifelse(target_MonitoringType %in% "Mesic Oak", 80,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 70,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 70, NA)))))

target_Shrub <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 30,
         ifelse(target_MonitoringType %in% "Xeric Oak", 20,
                ifelse(target_MonitoringType %in% "Mesic Oak", 30,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 20,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 10, 
                                     ifelse(target_type == "Grassland", 75, NA))))))

target_Native <- 
  ifelse(target_MonitoringType %in% "Pine Oak", 50,
         ifelse(target_MonitoringType %in% "Xeric Oak", 40,
                ifelse(target_MonitoringType %in% "Mesic Oak", 25,
                       ifelse(target_MonitoringType %in% "Oak Hickory", 40,
                              ifelse(target_MonitoringType %in% "Mixed Oak", 40, 
                                                   ifelse(target_type == "Grassland", 75, NA))))))


### Grasslands 

target_Woody <- 
  ifelse(target_type %in% "Grassland", 25, NA)

target_Invasive <- 
  ifelse(target_type %in% "Grassland", 25, NA)

# target_Native <- 
#   ifelse(target_type %in% "Grassland", 75, NA)


################################################################################
# TABLES
################################################################################


# Objectives_Forest <- data.frame(
#   "Num" = c(1,2,3,4,5,6),
#   "Objective" = c(
#     paste0("1. Maintain fine fuel loading (litter, 1-100hr fuels) at < ", target_FineFuelLoading, " tons/acre"),
#     paste0("2. Maintain duff depth to ≤ ", target_Duff, " inches"),
#     paste0("3. Increase sapling density to ≥ ", target_Sapling, " stems/acre"),
#     paste0("4. Reduce pole-sized tree density of mesic fire-intolerant species relative to xeric fire-adapted species"),
#     paste0("5. Maintain a stocking density of overstory trees between ", target_OverstoryLow, "-", target_OverstoryHigh, "%"),
#     paste0("6. Maintain or increase native grass and forb cover to ≥ ", target_Native, "%")
#   ))
# 
# Objectives_Grassland <- data.frame(
#   "Num" = c(1,2,3),
#   "Objective" = c(
#     paste0("1. Reduce woody cover to < ", target_Woody, "%"),
#     paste0("2. Reduce non-native invasive grass and forb cover to <", target_Invasive, "%"),
#     paste0("3. Increase native grass and forb cover to > ", target_Native, "%")
#   ))
