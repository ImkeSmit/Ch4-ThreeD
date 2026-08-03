###Descriptive statistics and generating figures for WP 1 progress report####
library(openxlsx)
library(tidyverse)
library(tidylog)
library(ggplot2)

veg25 <- read.xlsx("All_data/clean_data/community_2025.xlsx")
veg26 <- read.xlsx("All_data/clean_data/community_2026.xlsx")

#Check that all plots are here
nrow(distinct(veg25, turfID))
nrow(distinct(veg26, turfID))


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
  group_by(turfID, Year) |> 
  distinct(Species, .keep_all = TRUE) |> 
  mutate(sprich = n()) |> 
  ungroup() |> 
  select(-c(Species, `1`:`25`, Cover, Remark))|> 
  distinct(turfID, Year, .keep_all = TRUE)


delta <- sprichness |> 
  pivot_wider(id_cols = c(turfID, warming, grazing, Nlevel), names_from = Year, values_from = sprich) |> 
  mutate(delta_sprich = `2025`-`2026`)


warming_grazing_plot <- delta |> 
  mutate(grazing = factor(grazing, levels = c("C", "M", "I", "N"))) |> 
  ggplot(aes(x = warming, y = delta_sprich, fill = grazing)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()+
  scale_fill_manual(values = c("grey","lightgreen", "darkgreen",  "brown"), 
                    labels = c("C" = "Control", "M" = "Medium", "I" = "Intensive", "N" = "Natural")) +
  scale_x_discrete(labels = c("A" = "Ambient", "W" = "Warmed")) +
  labs(x = " ", y = "Change in species richness", fill = "Grazing treatment") +
  theme(panel.grid = element_blank())


warming_nitrogen_plot <- delta |> 
  mutate(Nlevel = case_when(Nlevel %in% c(1,2,3) ~ 0, .default = Nlevel)) |> 
  mutate(Nlevel = factor(Nlevel, levels = c(0, 4,5,6,7,8,9,10))) |>
  ggplot(aes(x = Nlevel, y = delta_sprich, fill = warming)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Change in species richness", x = "Nitrogen addition (kg/Ha)", fill = "Warming treatment")+
  scale_fill_manual(values = c("grey", "brown"), labels = c("A" = "Ambient", 'W' = "Warmed"))+
  scale_x_discrete(labels = c("4" = "0.5", "5" = "1", "6" = "5", "7" = "10", "8" = "50", "9" = "100", "10" = "150")) +
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom")


###Biomass data
nov25 <- read.csv("All_data/raw_data/2025/Biomass November 2025.csv") 
jan26 <- read.csv("All_data/raw_data/2026/Biomass/Biomass January 2026.csv")
feb26 <- read.csv("All_data/raw_data/2026/Biomass/Biomass February 2026.csv")
mar26 <- read.csv("All_data/raw_data/2026/Biomass/Biomass March 2026.csv")

biomass_all <- nov25 |> 
  bind_rows(jan26) |> 
  bind_

