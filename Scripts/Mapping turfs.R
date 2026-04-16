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
  pdf("Figures/turfmap.pdf")
x <- all_years %>% 
  filter(Species %in% c("Festuca scabra", "Tenaxia disticha", "Ficinia cinnamomea", "Sporobolus centrifugus")
         #grepl(c("Helichrysum", "Festuca"), Species),
         
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
    .y = glue::glue("Site {.$site_id}: plot {.$origPlotID}: turf {.$turf_id}"),
    .f = ~make_turf_plot(
      data = .x, 
      year = year, 
      species = species, 
      cover = cover, 
      subturf = subturf, 
      title = glue::glue(.y), 
      grid_long = grid)
  )} %>% 
  walk(print)#There are some plots that only print one year's data. I think it's because of a discrepancy between the origplotID's in the 2025 and 2026 data. fix that in the data cleaning

dev.off()



x2 <- all_years |>
  # sort
  arrange(destSiteID, origPlotID) |>
  group_by(destSiteID, origPlotID) |>
  rename(year = Year, 
         species = Species, 
         cover = Cover, 
         site_id = destSiteID,
         turf_id = turfID,
         plot = origPlotID) |> 
  nest() |>
  pmap(.f = \(site, plot, data){
    make_turf_plot(
      data = data,
      year = year, species = species, cover = cover, subturf = subturf,
      title = glue::glue("Site {site_id}: plot {plot}"),
      grid_long = grid
    )
  }) |>
  walk(print) # print all maps
