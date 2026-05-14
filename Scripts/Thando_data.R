###Script to separate data for Lindo's Honours####
library(openxlsx)
library(tidyverse)
library(tidylog)

#import 2025 and 2026 veg survey data
veg25 <- read.xlsx("All_data/clean_data/community_2025.xlsx") 
  
veg26 <- read.xlsx("All_data/clean_data/community_2026.xlsx")

T25 <- veg25 |> 
  filter(warming == "A", 
         grazing == "N", 
         Nlevel %in% c(1,2,3)) |> 
  select(-c(Remark, file, change_tracker, origSiteID, origBlockID, origPlotID, Scribe))

write.csv(T25, "All_data/clean_data/Lindo_community_2025.csv")


T26 <- veg26 |> 
  filter(warming == "A", 
         grazing == "N", 
         Nlevel %in% c(1,2,3)) |> 
  select(-c(Remark, file, change_tracker, origSiteID, origBlockID, origPlotID, Scribe))

write.csv(T26, "All_data/clean_data/Lindo_community_2026.csv")

  