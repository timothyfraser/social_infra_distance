#' @name 01_revision.R
#' @description Script for supplemental analyses for revise-and-resubmit of article. 
#' 



# Q1: DESCRIPTIVES #####################################
# What's the relationship between
#  - rates of each type of social infra and
#  - wealth

library(sf)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

m = list(
  path_site = "raw_data/sites.rds",
  path_grid = "raw_data/grid.rds",
  url_site = "https://raw.githubusercontent.com/timothyfraser/trust_but_verify/main/query/social_infrastructure_sites_boston.csv",
  url_grid = "https://raw.githubusercontent.com/timothyfraser/trust_but_verify/main/shapes/grid_covariates.geojson"
)

m$url_site %>% 
  read_csv() %>%
  # Convert to sf format,
  st_as_sf(
    # using the x and y columns to make a sf point object
    coords = c("x", "y"), 
    # setting the Coordinate Reference System (CRS) to 4326, 
    # which is the World Global System 1984 projection.
    crs = 4326) %>%
  # Filter to just points that were harvested from Google
  #filter(source == "googlapi") %>%
  # Filter to just these common types of social infrastructure
  filter(type %in% c("Parks", "Social Businesses", 
                     "Community Spaces", "Places of Worship"))  %>%
  # Save as an R Data file ".rds", which is easier for use. Use read_rds() to load.
  saveRDS(m$path_site)

m$path_site %>% read_rds() %>% nrow()

m$url_grid %>%
  read_sf() %>%
  # Grab these variables
  select(cell_id:linking, democrat_percent:geometry) %>%
  saveRDS(m$path_grid)



data = m$path_grid %>%
  read_rds() %>%
  filter(milestone != "") %>%
  select(cell_id) %>%
  st_join(
    y = m$path_site %>% 
      read_rds() %>% 
      select(type, geometry) %>% 
      filter(!is.na(type))) %>%
  as_tibble() %>%
  group_by(cell_id) %>% 
  reframe(community_space = sum(type == "Community Spaces", na.rm = TRUE),
          park = sum(type == "Parks", na.rm=TRUE),
          place_of_worship = sum(type == "Places of Worship", na.rm = TRUE),
          social_business = sum(type == "Social Businesses", na.rm = TRUE),
          total = community_space + park + place_of_worship + social_business) %>%
  ungroup() %>%
  left_join(by = "cell_id",
            y = m$path_grid %>%
              read_rds() %>% 
              as_tibble() %>% 
              mutate(pop_nonwhite = 1 - pop_white) %>%
              select(cell_id, milestone, pop_density_int, pop_white, pop_nonwhite, median_income)) %>%
  # Look at only populated regions of the city
  filter(pop_density_int > 0) %>%
 # Pivot longer into tidy form for fast calculation
  pivot_longer(cols = c(community_space:total), names_to = "type", values_to = "count") %>%
  # Estimate rate of sites using interpolated population density
  group_by(type) %>%  
  mutate(rate = count / pop_density_int * 1000) %>%
  ungroup()

data %>%
  group_by(type) %>%
  summarize(
    r_white = cor(log(rate+1), pop_white, use = "pairwise.complete.obs") %>% round(2),
    r_income = cor(log(rate +1), log(median_income), use = "pairwise.complete.obs") %>% round(2),
    count = sum(count, na.rm = TRUE),
    rate = mean(rate, na.rm = TRUE) %>% round(2),
  )

data %>%
  filter(type == "total") %>%
  summarize(income = median(median_income, na.rm = TRUE),
            white = median(pop_white, na.rm = TRUE))

read_rds("building_dist_dataset.rds")
