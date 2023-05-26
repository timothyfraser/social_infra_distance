#' @name ZZ_make_dataverse.R
#' @description Script for making and saving big files on the dataverse.
#' Some files are too big to store on github, so we've saved them in the Harvard Dataverse. Use this code to retrieve them.
#' For use by repository owner only. Readers will not need this.
#' 
# install.packages("dataverse")
library(dataverse)
library(dplyr)

# First, create a Harvard Dataverse API key and load it using a keys.R file.
# For security reasons, we do not share our API key, 
# but you can get yours and put it into a file named `00_keys.R`.
# We've provdided a template for you in "00_keys_template.R"

# Then, you can run it using:
source("00_keys.R")

key = Sys.getenv("DATAVERSE_KEY")
server = Sys.getenv("DATAVERSE_SERVER")

# Get a Harvard Dataverse API key
title =  "Replication Data for: How Far I’ll Go: Social Infrastructure Accessibility and Proximity in Urban Neighborhoods"
initiate_sword_dataset(
  body = list(
    title = title, 
    description = "Recent studies have shown that social infrastructure - community spaces, places of worship, social businesses, and parks - help build social ties, mitigate political polarization, and improve our health. This exploratory study examines why some areas in American cities see greater access to social infrastructure than others, applying geospatial models and city block analyses to the case of Boston. We hypothesize that neighborhoods of color and lower-income neighborhoods might see lower levels of access to social infrastructure, which have significant implications for the health and resilience of these neighborhoods. Using analyses of proximity on a large-N dataset of (1) all social infrastructure sites in Boston and (2) every building in Boston, we measure the distance between every building and social infrastructure site in the city, and model the associations of neighborhood demographics, socioeconomic status, and urban design factors on median distance from social infrastructure. Finally, we demonstrate findings using hyperlocal case studies of 3 iconic city blocks in Boston, examining Back Bay, Nubian Square, and Mount Bowdoin. We find evidence of strong race- and income-related gaps in access to social infrastructure, consistent even after extended controls for demographics and physical infrastructure traits, highlighting major inequities in social policy.", 
    creator = "Timothy Fraser"),
  dataverse = "timothyfraser", 
  key = key, 
  server = server)

# Your file can now be found here!
url = "https://doi.org/10.7910/DVN/ZEPG5R"
doi = url %>% stringr::str_remove(".*doi.org/") %>% paste0("doi:", .)

# Save these files to dataverse
files = c("raw_data/buffer_buildings_2000.rds",
          'raw_data/buffer_buildings_1500.rds',
          'raw_data/buffer_buildings_500.rds',
          'raw_data/buffer_buildings_100.rds',
          'raw_data/buffer_buildings_0.rds',
          'raw_data/buffer_dist.rds',
          'raw_data/buffer_dist_100.rds',
          'raw_data/buildings.rds')
for(i in 2:length(files)){
add_dataset_file(file = files[i], dataset = doi, key = key, server = server)
print(i)
}

rm(list = ls())

