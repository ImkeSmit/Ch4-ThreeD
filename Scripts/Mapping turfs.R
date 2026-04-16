###Script to map species in turfs over time
#Install turfmapper package
library(remotes)
remotes::install_github("Between-the-Fjords/turfmapper")
library(turfmapper)
library(openxlsx)
library(tidyverse)

comm25 <- read.xlsx("All_data/clean_data/community_2025.xlsx")
comm26 <- read.xlsx("All_data/clean_data/community_2026.xlsx")

all_years <- bind_rows(comm25,comm26) |> 
  pivot_longer(cols = 14:38, names_to = "subturf", values_to = "Presence", 
               names_transform = list(Subturf = as.integer)) |> 
  filter(Presence != "0") |> 
  mutate(subturf = as.numeric(subturf)) ##subturf names must be numeric
  
grid<- make_grid(ncol = 5)
plot_subturf_grid(grid_long = grid)

#plot one turf
all_years |>
  rename(year = Year, 
         species = Species, 
         cover = Cover, 
         site_id = destSiteID,
         turf_id = turfID)
  filter(turf_id == "139_AN8I_139") |> 
  make_turf_plot(
    data = _,
    year = year, species = species, cover = cover, 
    subturf = subturf,
    site_id = site_id,
    turf_id = turf_id,
    grid_long = grid
  )


#plot many turfs
x <- all_years %>% 
  filter(
         grepl("Helichrysum", Species),
         turfID %in% c("67_AN9M_67"),
  ) %>%
  arrange(destSiteID, origPlotID, turfID) %>% 
  group_by(destSiteID, origPlotID, turfID) %>% 
  rename(year = Year, 
         species = Species, 
         cover = Cover, 
         site_id = destSiteID,
         turf_id = turfID) %>%
  nest() %>% 
  {map2(
    .x = .$data, 
    .y = glue::glue("Site {.$destSiteID}: plot {.$destPlotID}: turf {.$turfID}"),
    .f = ~make_turf_plot(
      data = .x, 
      year = year, 
      species = species, 
      cover = cover, 
      subturf = subturf, 
      title = glue::glue(.y), 
      grid_long = grid)
  )} %>% 
  walk(print)
