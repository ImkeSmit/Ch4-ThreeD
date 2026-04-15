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


####
#create function to change incorrect names
standardise_names <- function(data, #dataframe containing species names that need to be corrected
                              data_species_column, #column in data contain the speciesnames, must be a string
                              #species names must be in the format genus_species.
                              #I.e. no capitals, with an underscore separating genus and specific epithet
                              naming_system,  #dataframe containing the old names that need to be changed, and the names they should be changed to
                              correct_name,  #column in naming_system that has the correct name
                              synonym, #columns in naming_system that has the synonyms
                              condition) #date condition to adhere to when changing names
{
  #add change tracker column to keep track of names changed
  data$change_tracker <- NA
  
  #remove names that do not have synonyms
  naming_system <- naming_system |> 
    filter(!is.na(synonym1))
  
  for (i in 1:nrow(data)) {
    old_name <- data[i, which(colnames(data) == data_species_column)]
    new_name <- NA
    
    found <- FALSE
    for (j in 1:nrow(naming_system)) { # looks whether species name should be corrected and replaces it with the new name
      found <- any(old_name == as.character(naming_system[j, synonym]))
      
      
      if (TRUE %in% found){ # only runs if the species is a synonym
        new_name <- naming_system[j, which(colnames(naming_system) %in% correct_name)] # finds the true name of the species and saves it
        record_date <- data[i, which(colnames(data) == "Date")] #get date of
        
        if(is.na(condition)){
        
        data[i, which(colnames(data) == data_species_column)] <- new_name
        
        #add column to keep track of which names changed
        data[i, which(colnames(data) == "change_tracker")] <- paste0(old_name, " -> ", new_name)
        
        } else {
          ###Only change the name if condition is met
          if(record_date < condition) { #only change name if date is before the condition date
            data[i, which(colnames(data) == data_species_column)] <- new_name
            #add column to keep track of which names changed
            data[i, which(colnames(data) == "change_tracker")] <- paste0(old_name, " -> ", new_name)
          }
        }
      }
    }
    
    
  }#end loop through rows
  return(data)
}#end function