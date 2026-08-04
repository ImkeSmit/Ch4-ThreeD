###Descriptive statistics and generating figures for WP 1 progress report####
library(openxlsx)
library(tidyverse)
library(tidylog)
library(ggplot2)

veg25 <- read.xlsx("All_data/clean_data/community_2025.xlsx")
veg26 <- read.xlsx("All_data/clean_data/community_2026.xlsx")

#import metaturfID
metaturfID <- read.xlsx("All_data/clean_data/metaTurfID.xlsx")

#Check that all plots are here
nrow(distinct(veg25, turfID))
nrow(distinct(veg26, turfID))


veg_all <- veg25 |> 
  bind_rows(veg26) |> 
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
  theme(panel.grid = element_blank(), axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14))
ggsave("sprichness_warming_grazing_boxplot.png", warming_grazing_plot, path = "Figures", width = 1500, height = 1300, units = "px")


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
  theme(legend.position = "bottom", panel.grid = element_blank(), axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14))
ggsave("sprichness_warming_nitrogen_boxplot.png", warming_nitrogen_plot, path = "Figures", width = 1500, height = 1300, units = "px")



###Which species had the largest decrease in cover?
sp_delta_cov <- veg_all |> 
  select(turfID,warming, grazing, Nlevel, Species, Year, Cover) |> 
  pivot_wider(id_cols = c(turfID, warming, grazing, Nlevel, Species), names_from = Year, values_from = Cover, values_fn = sum) |> 
  mutate(`2025` = case_when(is.na(`2025`) ~ 0, .default = `2025`), 
         `2026` = case_when(is.na(`2026`) ~ 0, .default = `2026`), 
         delta_cov = `2026` - `2025`) |> 
  group_by(Species) |> 
  summarise(mean_delta = mean(delta_cov)) |> 
  arrange(mean_delta)


###Biomass data
nov25 <- read.csv("All_data/raw_data/2025/Biomass November 2025.csv") |> 
  filter(!is.na(Graminoids..g.))
colnames(nov25) <-  c("destSiteID", "destBlockID", "turfID", "Graminoids", "Forbs", "Woody", "Date")
nov25x <- nov25 |> 
  select(destSiteID:Date) |>
  mutate(Date = ymd(Date)) |> 
  pivot_longer(cols = c(Graminoids:Woody), names_to = "category", values_to = "biomass_nov")

jan26 <- read.csv("All_data/raw_data/2026/Biomass/Biomass January 2026.csv")|> 
  filter(!is.na(Graminoids))
colnames(jan26) <-  c("destSiteID", "destBlockID", "turfID", "Graminoids", "Forbs", "Woody", "Date")
jan26x <- jan26 |> 
  select(destSiteID:Date) |>
  mutate(Date = dmy(Date)) |> 
  pivot_longer(cols = c(Graminoids:Woody), names_to = "category", values_to = "biomass_jan")


feb26 <- read.csv("All_data/raw_data/2026/Biomass/Biomass February 2026.csv")|> 
  filter(!is.na(Graminoids))
colnames(feb26) <-  c("destSiteID", "destBlockID", "turfID", "Graminoids", "Forbs", "Woody", "Date")
feb26x <- feb26 |> 
  select(destSiteID:Date) |>
  mutate(Date = dmy(Date)) |> 
  pivot_longer(cols = c(Graminoids:Woody), names_to = "category", values_to = "biomass_feb")

biomass_all <- nov25x |> 
  full_join(jan26x, by = c("turfID", "category")) |> 
  full_join(feb26x, by = c("turfID", "category")) |> 
  mutate(delta_biomass = biomass_feb - biomass_nov) |> 
  left_join(metaturfID, by = "turfID") |> 
  mutate(Nlevel = factor(Nlevel))


ggplot(biomass_all, aes(x = Nlevel, y = delta_biomass, fill = grazing)) +
  geom_boxplot()
  

bind_rows(jan26x) |> 
  bind_rows(feb26x) |> 
  select(destSiteID:Date) |>
  mutate(month = month(Date), 
         month = case_when(month == 1 ~ "Jan", 
                           month == 2 ~ "Feb", 
                           .default = "Nov"), 
         month = factor(month, levels = c("Nov", "Jan", "Feb"))) |> 
  left_join(metaturfID, by = "turfID")

biomass_all |> 
  filter(grazing == "I") |> 
  mutate(Nlevel = factor(Nlevel)) |> 
ggplot(aes(x = month, y = Graminoids, group = turfID, colour = Nlevel)) +
  geom_point()+
  geom_line() +
  scale_colour_manual(labels = c("1" = "0", "2" = "0", "3" = "0","4" = "0.5", "5" = "1", "6" = "5", "7" = "10", "8" = "50", "9" = "100", "10" = "150"), 
                      values = c("grey", "grey", "grey", "lightgreen", "lightgreen", "darkgreen", "darkgreen","darkgoldenrod", "darkgoldenrod", "brown"))+
  labs(y = "Graminoid biomass (g)", x = "Month", colour = "Nitrogen level (kg/Ha)")+
  theme_bw()+
  theme(panel.grid = element_blank())


biomass_all |> 
  filter(grazing == "I") |> 
  mutate(Nlevel = factor(Nlevel)) |> 
  ggplot(aes(x = month, y = Forbs, group = turfID, colour = Nlevel)) +
  geom_point()+
  geom_line() +
  scale_colour_manual(labels = c("1" = "0", "2" = "0", "3" = "0","4" = "0.5", "5" = "1", "6" = "5", "7" = "10", "8" = "50", "9" = "100", "10" = "150"), 
                      values = c("grey", "grey", "grey", "lightgreen", "lightgreen", "darkgreen", "darkgreen","darkgoldenrod", "darkgoldenrod", "brown"))+
  labs(y = "Forb biomass (g)", x = "Month", colour = "Nitrogen level (kg/Ha)")+
  theme_bw()+
  theme(panel.grid = element_blank())

