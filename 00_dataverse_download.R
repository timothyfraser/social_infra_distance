#' @name 00_dataverse_download.R
#' @description Second script to run.
#' Some files are too big to store on github, so we've saved them in the Harvard Dataverse. Use this code to retrieve them.


# install.packages("dataverse")
library(dataverse)
library(dplyr)
library(readr)
library(sf)

# First, create a Harvard Dataverse API key and load it using a keys.R file.
# For security reasons, we do not share our API key, 
# but you can get yours and put it into a file named `00_keys.R`.
# We've provdided a template for you in "00_keys_template.R"

# Then, you can run it using:
source("00_keys.R")

key = Sys.getenv("DATAVERSE_KEY")
server = Sys.getenv("DATAVERSE_SERVER")

# Get the doi for this repository
doi = "https://doi.org/10.7910/DVN/ZEPG5R" %>% 
  stringr::str_remove(".*doi.org/") %>% paste0("doi:", .)

f = tibble(
  files = c(
    "buffer_buildings_2000.rds",
    'buffer_buildings_1500.rds',
    'buffer_buildings_500.rds',
    'buffer_buildings_100.rds',
    'buffer_buildings_0.rds',
    'buffer_dist.rds',
    'buffer_dist_100.rds',
    'buildings.rds'),
  path = paste0("raw_data/", files))

# Download each file
for(i in 1:nrow(f)){
  get_dataframe_by_name(
    filename = f$files[i], dataset = doi, format = "original",.f = readr::read_rds,
    key = key, server = server) %>% 
    saveRDS(f$path[i])
  print(f$files[i])
}



