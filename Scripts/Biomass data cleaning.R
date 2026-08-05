####Script to clean biomass data
library(tidyverse)
library(tidylog)
library(openxlsx)

###Import metaturfID and filter to find teh plots that get clipping treatements
meta <- read.xlsx("All_data/clean_data/metaTurfID.xlsx") 

meta_M_I <- meta |> 
  filter(grazing %in% c("M", "I"))

meta_I <- meta |> 
  filter(grazing == "I")


#Import data from each month
###NOVEMBER 2025####
nov25 <- read_csv("All_data/raw_data/2025/Biomass November 2025.csv",  
                  col_select = c(1:8), col_types = list("c", "c", "c", "d", "d", "d", "D", "c")) |> 
  rename(Graminoids = `Graminoids (g)`, 
         Forbs = `Forbs (g)`, 
         Woody = `Woody (g)`, 
         Date = `Date collected`, 
         Notes = ...8) |> 
  filter(!is.na(turfID))

#are all the turfID's here?
missing_nov25 <- nov25 |> 
  anti_join(meta_M_I, by = "turfID") #none missing

#do all turfs have at least a graminoid mass?
nov25 |> filter(is.na(Graminoids)) #all have a graminoid mass
nov25 |> filter(is.na(Forbs)) #all have a forb mass


####january 2026####
jan26 <- read_csv("All_data/raw_data/2026/Biomass/Biomass January 2026.csv",  
                  col_select = c(1:8), col_types = list("c", "c", "c", "d", "d", "d", "c", "c")) |> 
  rename(Date = `Date collected`, 
         Notes = ...8) |> 
  filter(!is.na(turfID)) |> 
  mutate(Date = dmy(Date))

#are all the turfID's here?
missing_jan26 <- jan26 |> 
  anti_join(meta_I, by = "turfID") #none missing

#do all turfs have at least a graminoid mass?
jan26 |> filter(is.na(Graminoids)) #all have a graminoid mass
jan26 |> filter(is.na(Forbs)) #all have a forb mass


####FEBRUARY 2026####
feb26 <- read_csv("All_data/raw_data/2026/Biomass/Biomass February 2026.csv",  
                  col_select = c(1:8), col_types = list("c", "c", "c", "d", "d", "d", "c", "c")) |> 
  rename(Notes = ...8) |> 
  filter(!is.na(turfID)) |> 
  mutate(Date = dmy(Date), 
         Date = case_when(is.na(Date) ~ dmy("23/02/2026"), .default = Date))

#are all the turfID's here?
missing_feb26 <- feb26 |> 
  anti_join(meta_M_I, by = "turfID") #none missing

#do all turfs have at least a graminoid mass?
feb26 |> filter(is.na(Graminoids)) #missing!
feb26 |> filter(is.na(Forbs)) #missing!