###Script to separate data for Thando's Honours####
library(openxlsx)

#import 2025 and 2026 veg survey data
veg25 <- read.xlsx("All_data/clean_data/community_2025.xlsx")
veg26 <- read.xlsx("All_data/clean_data/community_2026.xlsx")
