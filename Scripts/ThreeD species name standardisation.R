##Cleaning Three-D species names for all years
#let's get species lists for 2025 and 2026
library(openxlsx)

#Create a name key
sp_25 <- read.xlsx("All_data/clean_data/community_2025.xlsx")
sp_26 <- read.xlsx("All_data/clean_data/community_2026.xlsx")

taxa_2025 = unique(sp_25$Species)[order(unique(sp_25$Species))]
taxa_2026 = unique(sp_26$Species)[order(unique(sp_26$Species))]
#make them the same length
taxa_2025 = c(taxa_2025, rep(NA, c(length(taxa_2026) - length(taxa_2025))))

splist = data.frame(taxa_2025 = taxa_2025, taxa_2026 = taxa_2026)

#save the name key
write.xlsx(splist, 'All_data/clean_data/threed_name_key.xlsx')
