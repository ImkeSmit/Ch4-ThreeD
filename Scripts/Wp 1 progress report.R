###Descriptive statistics and generating figures for WP 1 progress report####
library(openxlsx)
library(tidyverse)
library(tidylog)

veg25 <- read.xlsx("All_data/clean_data/community_2025.xlsx")
veg26 <- read.xlsx("All_data/clean_data/community_2026.xlsx")

veg_all <- veg25 |> 
  bind_rows(veg_26) |> 
  tibble()

###How many species each year?
veg_all |> 
  group_by(Year) |> 
  distinct(Species) |> 
  summarise(nsp = n())

###Changes in Species richness per treatment
sprichness <- veg_all |> 
  group_by(turfID) |> 
  distinct(Species, .keep_all = TRUE) |> 
  mutate(sprichness = n()) |> 
  ungroup() |> 
  select(-c(Species, `1`:`25`, Cover, Remark))|> 
  distinct(turfID, .keep_all = TRUE)



