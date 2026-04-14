#import and clean community data from Three_D experiment
#2026 VEGETATION SURVEY
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringi)

####CREATE METADATA####
#Create metadata file detailing the treatments associated with each turfID
# high and mid
origSiteID <-  c("high", "mid")
origBlockID <-  c(1:10)
origPlotID <- tibble(origPlotID = 1:160)
warming <-  c("A", "W")
grazing <-  c("C", "M", "I", "N")
# Nitrogen level needs to be in a certain order
nitrogen <- tibble(Nlevel = rep(rep(c(1,6,5,3,10,7,4,8,9,2), each = 8), 2))
# add corresponding N amount in kg per ha and year
NitrogenDictionary <- tibble(Nlevel = c(1,6,5,3,10,7,4,8,9,2),
                             Namount_kg_ha_y = c(0, 5, 1, 0, 150, 10, 0.5, 50, 100, 0))

# cross site, block warm and grazing treatment
meta <- crossing(origSiteID, origBlockID, warming, grazing) %>% 
  bind_cols(nitrogen)

# Vik (is done separately because it is only destination site)
vik <- tibble(
  origSiteID = factor("low", levels = c("high", "mid", "low")),
  origBlockID = rep(1:10, each = 4),
  origPlotID = 161:200,
  destSiteID = factor(NA, levels = c("high", "mid", "low")),
  Nlevel = rep(c(1,6,5,3,10,7,4,8,9,2), each = 4),
  warming = "W",
  grazing = rep(c("notN", "notN", "notN", "N"), 10),
  fence = if_else(grazing == "N", "out", "in"))

# randomize warming and grazing treatment
set.seed(32) # seed is needed to replicate sample_frac
meta2 <- meta %>% 
  # create variable for grazing treatment inside or outside fence
  mutate(fence = if_else(grazing == "N", "out", "in")) %>% 
  mutate(origSiteID = factor(origSiteID, levels = c("high", "mid", "low"))) %>%
  arrange(origSiteID) %>% # site needs to be arranged, because transplant goes only in one direction
  group_by(origSiteID, origBlockID, Nlevel, fence) %>%
  sample_frac() %>% # randomization
  ungroup() %>% 
  bind_cols(origPlotID) %>% # add plotID
  mutate(destSiteID = case_when(
    origSiteID == "high" & warming == "A" ~ "high",
    origSiteID == "mid" & warming == "W" ~ "low",
    TRUE ~ "mid")) %>%
  mutate(destSiteID = factor(destSiteID, levels = c("high", "mid", "low"))) %>%
  bind_rows(vik) %>% # add Vik
  group_by(origSiteID, origBlockID, warming, fence) %>% 
  mutate(rownr = row_number())


# Join meta2 to warmed plots
metaTurfID <- left_join(
  meta2 %>% filter(origPlotID < 161), # remove plots from vik
  # only warmed plots, remove unused rows
  meta2 %>% filter(warming == "W") %>% select(-grazing, -destSiteID, destPlotID = origPlotID), 
  by = c("destSiteID" = "origSiteID", "origBlockID" = "origBlockID", "rownr" = "rownr", "fence" = "fence", "Nlevel" = "Nlevel", "warming" = "warming"), 
  suffix = c("", "_dest")) %>% 
  mutate(destBlockID = origBlockID,
         destPlotID = ifelse(is.na(destPlotID), origPlotID, destPlotID),
         turfID = paste0(origPlotID, "_", warming, "N", Nlevel, grazing,  "_", destPlotID)) %>% 
  ungroup() %>% 
  select(-fence, -rownr) %>% 
  #CHANGE PLOTID 23-103 TO 23 AMBIENT, AND 24 TO 24-103 WARMING (wrong turf was transplanted!)
  mutate(warming = ifelse(origSiteID == "high" & origPlotID == 23, "A", warming),
         destPlotID = ifelse(origSiteID == "high" & origPlotID == 23, 23, destPlotID),
         turfID = ifelse(origSiteID == "high" & origPlotID == 23, "23_AN5N_23", turfID),
         
         warming = ifelse(origSiteID == "high" & origPlotID == 24, "W", warming),
         destPlotID = ifelse(origSiteID == "high" & origPlotID == 24, 103, destPlotID),
         turfID = ifelse(origSiteID == "high" & origPlotID == 24, "24_WN5N_103", turfID)) %>% 
  mutate(destSiteID = as.character(destSiteID)) %>% 
  mutate(destSiteID = case_when(turfID == "23_AN5N_23" ~ "high",
                                turfID == "24_WN5N_103" ~ "mid",
                                TRUE ~ destSiteID))

write.xlsx(metaTurfID, file = "All_data/clean_data/threed/metaTurfID.xlsx", colNames = TRUE)


####IMPORT VEG SURVEY DATA####
#run import_community2026 script first
#script is custom for 2026 data
metadat <- read.xlsx("All_data/clean_data/metaTurfID.xlsx", colNames = T)

veg2026 <- import_community_2026(metadat, filepath = "All_data/raw_data/2026/vegetation_survey")
#This import community function is not hard coded for 2026, it should be generic enough to use for all years

####Clean community data ####
veg_only <- veg2026 |> #remove other variables besides veg cover
  filter(!Species %in% c("Total Cover (%)","Vascular plants","Bryophyes","Lichen", "Litter","Bare soil",
                         "Bare rock","Poop","Height / depth (cm)","Vascular plant layer","Moss layer" , "Subplot recording (highest level):")) |> 
  mutate(Cover = as.numeric(Cover)) |> 
  filter(!is.na(Species)) |> 
  #remove rows with no entries in any of the 25 subcells
  filter(!if_all(`1`:Cover, is.na))

#Check if all the turfs are here
length(unique(veg_only$turfID)) #160 unique turfID's
#find wrong turfID's
entered_turfs <- data.frame(turfID = unique(veg_only$turfID))
metadat_turfs <- data.frame(turfID = unique(metadat$turfID))

only_in_entered <- anti_join(entered_turfs, metadat_turfs, by = "turfID")
only_in_metadat <- anti_join(metadat_turfs, entered_turfs, by = "turfID")

#First, let's fix some turf naming mistakes
veg_only2 <- veg_only |> 
  mutate(turfID = ifelse(turfID == "24_WN10N_103", "24_WN5N_103", turfID),
         turfID = ifelse(turfID == "29_ WN3C_106", "29_WN3C_106", turfID), 
         turfID = ifelse(turfID == "85 WN1C 162", "85_WN1C_162", turfID)) |> 
        ##Add the site and blockID etc for these turfs
        left_join(metadat, by = "turfID", suffix = c("", "_fill")) #give the columns from metadat the suffix -fill

#For each column in table1, coalesce NA values with table2's values
cols_to_fill <- names(veg_only)[c(1:7)]#get the columns where we need to fill values in

veg_only3 <- veg_only2 %>%
  mutate(across(
    all_of(cols_to_fill),
    ~ coalesce(.x, get(paste0(cur_column(), "_fill"))) #fill the cell with the first non-NA value from the variable in the two tables
  )) %>%
  select(names(veg_only))  # Drop the helper "_fill" columns


#There are a few plots with no total cover measurements. Add them from looking at the pictures. 
#which plots are missing % cover for species
#NA cover values are allowed because the dead plants have cover = NA
addcov <- veg_only3 |> 
  group_by(turfID) |> 
  summarise(coversum = sum(Cover, na.rm = T)) |> 
  filter(coversum == 0) #none missing

#Let's check if only dead sp have NA cover
nacov <- veg_only3 |> 
  filter(is.na(Cover)) |> 
  select(Species, turfID, Cover) #There are a few that are supposed to have cover values


#remove the unknown seedlings row if it has no entries
veg_only4 <- veg_only3 |> 
  #remove the row if thenumber of NA's equal the number of columns
  filter(!(Species == "Unknown seedlings" & 
             rowSums(is.na(across(14:38))) == (38 - 14 + 1))) |> 
  #Give the Cover value of 0.5 if there is an entry in of 1 in any of the 25 columns
  mutate(Cover = case_when(Species == "Unknown seedlings" & 
                            rowSums(is.na(across(14:38))) < (38 - 14 + 1) ~ 0.5, 
                           .default = Cover)) |> 
  #there is one more row with Cover = NA. 
  #This is for turf 60_AN8M_60 for Heliophila rigidiscula
  #Safe to just give it 0.5
  mutate(Cover = case_when(Species == "Heliophila rigidiscula" & turfID == "60_AN8M_60" ~ 0.5, 
                           .default = Cover))

##Overwrite NA's with zeroes
replace_cols = colnames(veg_only3)[14:38]
for(r in 1:nrow(veg_only3)) {
  
  for(i in 1:length(replace_cols)) {
    
    element <- veg_only3[r, which(colnames(veg_only3) == replace_cols[i])]
    
    if(is.na(element)) {
      veg_only3[r, which(colnames(veg_only3) == replace_cols[i])] <- "0"
    }
  }
}



write.xlsx(veg_only3, "All_data/clean_data/threed/community_2025.xlsx")



####Clean the veg height and abiotic data####
abiotic_only <- veg2025 |> 
  filter(Species %in% c("Vascular plants","Bryophyes","Lichen", "Litter","Bare soil",
                         "Bare rock","Poop","Vascular plant layer","Moss layer" )) |>
  select(!Cover) |> 
  rename(Variable = Species) |> 
  #rename variable names
  mutate(Variable = case_when(Variable == "Vascular plants" ~ "Vascular plant cover", 
                              Variable == "Bryophyes" ~ "Bryophyte cover", 
                              Variable == "Lichen" ~ "Lichen cover",
                              Variable == "Litter" ~ "Litter cover",
                              Variable == "Bare soil" ~ "Bare soil cover",
                              Variable == "Bare rock" ~ "Bare rock cover", 
                              Variable == "Poop" ~ "Poop cover",
                              Variable == "Vascular plant layer" ~ "Vascular plant layer height",
                              Variable == "Moss layer" ~ "Moss layer height", 
                              .default = Variable)) |>
  mutate(across(c(`1`:`25`), as.numeric)) #make all numeric

#create table of vascular height
vasc_height <- abiotic_only |> 
  filter(Variable == "Vascular plant layer height") |> 
  select(turfID, Variable, `1`:`4`) |> 
  rename(Vascular_plant_height1 = `1`, 
         Vascular_plant_height2 = `2`,
         Vascular_plant_height3 = `3`,
         Vascular_plant_height4 = `4`) |> 
  select(!Variable)

#create table of moss height
moss_height <- abiotic_only |> 
  filter(Variable == "Moss layer height") |> 
  select(turfID, Variable, `1`:`4`) |> 
  rename(Moss_layer_height1 = `1`, 
         Moss_layer_height2 = `2`,
         Moss_layer_height3 = `3`,
         Moss_layer_height4 = `4`) |> 
  select(!Variable)

#join moss and vascular height to other data
abiotic_only2 <- abiotic_only |> 
  filter(!Variable %in% c("Vascular plant layer height", "Moss layer height")) |>
  left_join(vasc_height, by = "turfID") |> 
  left_join(moss_height, by = "turfID")


#identify problematic cover values
summary(abiotic_only2) #cols1 -25 must have max cover of 100.5
#col 10 has max of 999

prob10 <- abiotic_only2 |> 
  slice_max(`10`, n = 10) #problem value is at 93_AN6M_93
#fix it
abiotic_only2[which(abiotic_only2$turfID == "93_AN6M_93" & abiotic_only2$Variable == "Vascular plant cover"), 
              which(colnames(abiotic_only2) == "10")] <- 99



#replace NA's with 0 where appropriate
replace_cols = colnames(abiotic_only2)[14:38]
for(r in 1:nrow(abiotic_only2)) {
    
    for(i in 1:length(replace_cols)) {
      
      element <- abiotic_only2[r, which(colnames(abiotic_only2) == replace_cols[i])]
      
      if(is.na(element)) {
        abiotic_only2[r, which(colnames(abiotic_only2) == replace_cols[i])] <- 0
      }
    }
  }
  
write.xlsx(abiotic_only2, "All_data/clean_data/threed/abiotic_and_veg_height_2025.xlsx")

  
  
  
