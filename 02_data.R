#' @name 02_data.R
#' @description Build Datasets!


## Get buildings

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



m$path_grid %>%
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
  ungroup() %>%
  saveRDS("raw_data/rates.rds")


library(tidyverse)
library(sf)
read_sf("https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::boston-buildings.geojson?outSR=%7B%22latestWkid%22%3A2249%2C%22wkid%22%3A102686%7D") %>%
  select(building_id = BLDG_ID) %>%
  write_rds("raw_data/buildings.rds")

# Convert buildings to points
read_rds("raw_data/buildings.rds") %>%
  mutate(geometry = st_centroid(geometry)) %>%
  # Do an inner join to just keep buildings within this grid
  st_join(read_rds("raw_data/grid.rds") %>% select(cell_id, geometry), left = FALSE) %>%
  mutate(building_id = building_id %>% na_if("")) %>%
  filter(!is.na(building_id)) %>%
  write_rds("raw_data/buildings_points.rds")

## Get roads

library(tidyverse)
library(sf)

read_sf("https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::boston-street-segments.geojson?outSR=%7B%22latestWkid%22%3A2249%2C%22wkid%22%3A102686%7D") %>%
  select(street_id = STREET_ID, segment_id = SEGMENT_ID) %>%
  # Do an inner join to just keep roads within this grid
  st_join(read_rds("raw_data/grid.rds") %>% select(cell_id, geometry), left = FALSE) %>%
  write_rds("raw_data/streets.rds")


# Here's how you might visualize these all together
grid <- read_rds("raw_data/grid.rds") 
sites <- read_rds("raw_data/sites.rds")
buildings <- read_rds("raw_data/buildings_points.rds") # >40,000 points

ggplot() +
  geom_sf(data = grid) +
  geom_sf(data = buildings, color = "blue", alpha = 0.25) +
  geom_sf(data = sites)

rm(list = ls())



## Get Distance

# Next, let's try to replicate the distance analysis for every cell.


# Here's how you might visualize these all together
# Get the grid cell
grid <- read_rds("raw_data/grid.rds") 


# Next, let's get a jumbo dataset, where each row indicates a site-buffer pair, 
# showing every site that fell into every buffer.
# (In other words, some sites will show up multiple times, 
# captured by multiple buffers. That's okay - that's intended.) 
# Let's also import our buildings, to make a gigantic dataset of building-buffer pairs. 
# It's not clear which size of buffer we need, so we're going to do a bunch, all in a loop.


get_buffer = function(mydistance){
  # Let's make an ZZZZ circle around our grid cells
  buffer <- grid %>% 
    select(buffer_id = cell_id, geometry) %>%
    # Specify the distance of the perimeter, using 'mydistance'
    st_buffer(dist = mydistance)
  
  # Let's import ALL the social infrastructure sites located within 1 square kilometers of each grid cell
  read_rds("raw_data/sites.rds") %>%
    # Lets zoom in JUST to sites overlapping our 1 km buffer, doing an inner_join (as opposed to a left_join)
    st_join(buffer, left = FALSE) %>%
    # Let's keep a copy
    saveRDS(paste("raw_data/buffer_sites_", mydistance, ".rds", sep = ""))
  
  read_rds("raw_data/buildings_points.rds") %>%
    # Each matched to a buffer!
    # Lets zoom in JUST to sites overlapping our 1 km buffer, doing an inner_join (as opposed to a left_join)
    st_join(buffer, left = FALSE) %>%
    # let's keep a copy just in case
    saveRDS(paste("raw_data/buffer_buildings_", mydistance, ".rds", sep = ""))
  
  # Now save the buffer to file, in case we need it.
  buffer %>%
    saveRDS(paste("raw_data/buffer_", mydistance, ".rds", sep = ""))
  
}


# Originally, we ran the loop for 0, 100, 500, 1000, 1500, and 2000 meter buffers
# c(0, 100, 500, 1000, 1500, 2000) %>%
#   map(~get_buffer(.))

# But we really only need the loop for 1000 meters
get_buffer(1000)



# Now, we're going to do something called a ```function.``` 
# In a function, we write a loop of code that we ask the computer 
# to do several times in a row, given an input. Only the input changes, 
# so your choice of input matters - in this case, we will supply the function
# with each of our differnt ```buffer_id```, so that we can run this code many times, 
# once per buffer.


get_distance = function(mybuffer_id){
  require(tidyverse)
  require(sf)
  
  # Show progress
  print(mybuffer_id)
  
  # Import our subset data
  samplebuildings <- buildings %>%
    # Zoom into just buildings within our cell of interest
    filter(cell_id %in% mybuffer_id)
  
  samplesites <- sites %>%
    # Zoom into just sites within our the buffer of our cell of interest
    filter(buffer_id %in% mybuffer_id) %>% 
    select(id, type)
  
  # If we have any valid data in this cell to analyze, then do the following:
  if(length(samplebuildings$building_id) > 0 & length(samplesites$id) > 0){
    # Collect a set of unique identifier pairs for the lines we're about the make
# Use expand grid to get a complete set of buildings and sites,
# In the same order supplied to st_nearest_points
mydetails <-  expand_grid(
  # Grab the building ID
  samplebuildings %>% as_tibble() %>% select(building_id),
  # Grab the site id and type
  samplesites %>% as_tibble() %>% select(id, type))


mylines <- samplebuildings %>%
  # Please give me lines between my building and every one of our sites
  st_nearest_points(samplesites) %>%
  # Format as an sf object, with the WGS 84 coordinate reference system (code = 4326)
  # and rename the weird column to geometry
  st_as_sf(crs = 4326) %>% rename(geometry = x) %>%
  # Now calculate the distance of these lines, in meters!
  mutate(dist = st_length(geometry) %>% as.numeric()) %>%
  as_tibble() %>%
  select(dist) %>%
  # Bind in unique identifiers
  bind_cols(mydetails %>% select(building_id, type))

mylines %>%
  # For each type, per grid cell buffer,
  group_by(building_id, type) %>%
  # Calculate median distance using 10 thresholds,
  summarize(
    # let's get median distance as crow flies between buildings in this block 
    # and social infrastructure of this type 
    # within 100 feet of that building
    dist100 = median(dist[dist <= 100], na.rm = TRUE),
    # within 200 feet of that building
    dist200 = median(dist[dist <= 200], na.rm = TRUE),
    # within 300 feet of that building
    dist300 = median(dist[dist <= 300], na.rm = TRUE),
    # within 400 feet of that building
    dist400 = median(dist[dist <= 400], na.rm = TRUE),
    # et cetera
    dist500 = median(dist[dist <= 500], na.rm = TRUE),
    dist600 = median(dist[dist <= 600], na.rm = TRUE),
    dist700 = median(dist[dist <= 700], na.rm = TRUE),
    dist800 = median(dist[dist <= 800], na.rm = TRUE),
    dist900 = median(dist[dist <= 900], na.rm = TRUE),
    dist1000 = median(dist[dist <= 1000], na.rm = TRUE),
    # Also be sure to use no threshold once too
    dist = median(dist, na.rm = TRUE),
    
    # Let's also count the total sites that fall into that area!
    count100 = sum(!is.na(type[dist <= 100]), na.rm = TRUE),
    # within 200 feet of that building
    count200 = sum(!is.na(type[dist <= 200]), na.rm = TRUE),
    # within 300 feet of that building
    count300 = sum(!is.na(type[dist <= 300]), na.rm = TRUE),
    # within 400 feet of that building
    count400 = sum(!is.na(type[dist <= 400]), na.rm = TRUE),
    # et cetera
    count500 = sum(!is.na(type[dist <= 500]), na.rm = TRUE),
    count600 = sum(!is.na(type[dist <= 600]), na.rm = TRUE),
    count700 = sum(!is.na(type[dist <= 700]), na.rm = TRUE),
    count800 = sum(!is.na(type[dist <= 800]), na.rm = TRUE),
    count900 = sum(!is.na(type[dist <= 900]), na.rm = TRUE),
    count1000 = sum(!is.na(type[dist <= 1000]), na.rm = TRUE),
    # Also be sure to use no threshold once too
    count = sum(!is.na(type), na.rm = TRUE),
    
    # Finally, let's append that buffer ID
    buffer_id = mybuffer_id) %>%
  ungroup() %>%
  # Write that cell to file
  saveRDS(paste("count/", mybuffer_id, ".rds", sep = ""))

}
}

get_distance %>%
  saveRDS("raw_data/get_distance_function.rds")
remove(grid, buffer) 


# Finally, let's run the loop!

# First, let's load our data in.
buildings <- read_rds("raw_data/buildings.rds") %>%
  st_join(read_rds("raw_data/grid.rds") %>% select(cell_id))
buffer <- read_rds("raw_data/buffer_1000.rds")
sites <- read_rds("raw_data/buffer_sites_1000.rds") %>% select(id, type, buffer_id)
get_distance <- read_rds("raw_data/get_distance_function.rds")

# Make a folder to hold our results
dir.create("count")

# Note: I wouldn't recommend running this code. 
# It took me about 25 minute with 8GB of RAM. 
# A usual RStudio Cloud Project has 1 GB of RAM.

library(tidyverse)
library(sf)
library(future)
library(furrr)

plan("multisession")

buffer$buffer_id %>%
  #done <- str_remove(dir("count"), ".rds")
  #buffer$buffer_id[!buffer$buffer_id %in% done] %>%
  furrr::future_map(~get_distance(.), .progress = TRUE)

plan("sequential")

# Now bind the results together into one data.frame
tibble(file = dir("count", full.names = TRUE)) %>%
  filter(!str_detect(file, "Cell ")) %>%
  split(.$file) %>%
  map_dfr(~read_rds(.$file)) %>%
  saveRDS("raw_data/buffer_dist.rds")


rm(list = ls()); gc()



## Get Land Value and Type

# Let's also grab, as covariates, the type of zoning for each building
# and the cost of that building.

library(tidyverse)
library(sf)

# https://data.boston.gov/dataset/property-assessment/resource/c4b7331e-e213-45a5-adda-052e4dd31d41?inner_span=True

read_csv("https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/c4b7331e-e213-45a5-adda-052e4dd31d41/download/data2021-full.csv") %>% 
  magrittr::set_colnames(value = names(.) %>% tolower() %>% str_replace_all(" ", "_")) %>%
  select(pid, lu, own_occ, 
         total_value,land_value, bldg_value,
         gross_area, land_area = land_sf, bldg_area = living_area, 
         gross_tax, yr_built,yr_remodel, overall_cond) %>%
  # Convert land value to numeric, and calculate total value per square foot (of land and property)
  mutate_at(vars(total_value, land_value, bldg_value, gross_tax), list(~parse_number(.))) %>%
  # Convert categories to factor!
  mutate_at(vars(own_occ, lu, overall_cond), list(~factor(.))) %>%
     mutate(lu = lu %>% recode_factor(
    "A" = "Apartment Building (7 or more units)",
    "AH" = "Agricultural/Horticultural",
    "C" = "Commericial",
    "CC" = "Commercial Condo",
    "CD" = "Residential Condo",
    "CL" = "Commercial Land",
    "CM" = "Condo main structure",
    "CP" = "Condo parking",
    "E" = "Tax-Exempt",
    "EA" = "Tax-Exempt (blighted)",
    "I" = "Industrial",
    "R1" = "Residential 1-family",
    "R2" = "Residential 2-family",
    "R3" = "Residential 3-family",
    "R4" = "Residential 4+family",
    "RC" = "Mixed use",
    "RL" = "Residential Land")) %>%
    # Recode variable to an ordinal scale from 1 (unsound) to 8 (excellent)
   mutate(overall_cond = overall_cond %>% recode_factor(
    "EX - Excellent" = "8",
    "E - Excellent" = "8",
    "VG - Very Good" = "7",
    "G - Good" = "6",
    "AVG - Default - Average" = "5",
    "A - Average" = "5",
    "F - Fair" = "4",
    "P - Poor" = "3",
    "VP - Very Poor" = "2",
    "US - Unsound" = "1") %>% as.character() %>% as.numeric()) %>%
  # Turn owner occupied into a binary variable
  mutate(own_occ = if_else(own_occ == "Y", true = 1, false = 0, missing = NA_real_)) %>%
  # Calculate cost per square foot
  mutate(cost_sqft = total_value / gross_area) %>%
  # A couple records were duplicated. Take the median of each
  group_by(pid) %>%
  summarize(lu = unique(lu),
            own_occ = median(own_occ, na.rm = TRUE),
            cost_sqft = median(cost_sqft, na.rm = TRUE),
            overall_cond = median(overall_cond, na.rm = TRUE),
            yr_built = median(yr_built, na.rm = TRUE),
            gross_tax = median(gross_tax, na.rm = TRUE)) %>%
  ungroup() %>%
  write_rds("raw_data/buildings_value.rds")



## Get Transit

# We're also going to estimate distance of all buildings 
# from the nearest bus and train stop.


library(tidyverse)
library(sf)

train <- read_sf("raw_data/transit/MBTA_NODE.shp") %>%
  st_transform(crs = 4326) %>% select(station = STATION, line = LINE) %>%
  # make an extra geometry column
  mutate(geo_transit = geometry) 

bus <- read_sf("raw_data/transit/MBTABUSSTOPS_PT.shp") %>%
  st_transform(crs = 4326) %>% 
  select(stop_id = STOP_ID) %>%
  # make an extra geometry column
  mutate(geo_transit = geometry)

buildings <- read_rds("raw_data/buildings_points.rds") %>%
  rename(geo_building = geometry)

# Identify which train stop is closest to each building site
buildings %>%
  st_join(train, join = st_nearest_feature) %>%
  # Next, we're going to bind in the coordinates of 
  bind_cols(st_coordinates(.$geo_building) %>% as_tibble() %>% select(x1 = 1, y1 = 2),
            st_coordinates(.$geo_transit) %>% as_tibble() %>% select(x2 = 1, y2 = 2)) %>%
  # Filter out any locations that aren't quite right
  #filter(!is.na(x1) & !is.na(y1) & !is.na(x2) & !is.na(y2)) %>%
  select(building_id, line, x1:y2) %>%
  # Make a linestring
  mutate(geometry = sprintf("LINESTRING(%s %s, %s %s)", x1, y1, x2, y2)) %>%
  as_tibble() %>%
  select(building_id, line, geometry) %>%
  st_as_sf(wkt = "geometry", crs = 4326) %>%
  # And, let's calculate how far away it is!
  mutate(dist = st_length(geometry) %>% as.numeric()) %>%
  as_tibble() %>%
  select(building_id, train_dist = dist, train_nearest = line) %>%
  write_rds("raw_data/buildings_train.rds")


# Identify which bus stops is closest to each building site
buildings %>%
  st_join(bus, join = st_nearest_feature) %>%
  # Next, we're going to bind in the coordinates of 
  bind_cols(st_coordinates(.$geo_building) %>% as_tibble() %>% select(x1 = 1, y1 = 2),
            st_coordinates(.$geo_transit) %>% as_tibble() %>% select(x2 = 1, y2 = 2)) %>%
  # Filter out any locations that aren't quite right
  #filter(!is.na(x1) & !is.na(y1) & !is.na(x2) & !is.na(y2)) %>%
  select(building_id, stop_id, x1:y2) %>%
  # Make a linestring
  mutate(geometry = sprintf("LINESTRING(%s %s, %s %s)", x1, y1, x2, y2)) %>%
  as_tibble() %>%
  select(building_id, stop_id, geometry) %>%
  st_as_sf(wkt = "geometry", crs = 4326) %>%
  # And, let's calculate how far away it is!
  mutate(dist = st_length(geometry) %>% as.numeric()) %>%
  as_tibble() %>%
  select(building_id, bus_dist = dist, bus_nearest = stop_id) %>%
  write_rds("raw_data/buildings_bus.rds")

train <- read_rds("raw_data/buildings_train.rds")
bus <- read_rds("raw_data/buildings_bus.rds")

# The codes are identical, so we can bind_cols()
# sum(train$building_id == bus$building_id)
bind_cols(train, bus %>% select(bus_dist, bus_nearest)) %>%
  # Where we can access distance to closest bus or train
  write_rds("raw_data/buildings_transit.rds")

rm(list = ls())


## Get Buildings Data
library(tidyverse)
grid <- read_rds("raw_data/grid.rds") %>%
  as_tibble() %>%
  select(-geometry) %>%
  mutate(neighborhood = na_if(neighborhood, "")) %>%
  mutate(neighborhood = if_else(is.na(neighborhood), "Other", neighborhood))

dat <- read_rds("raw_data/buildings_value.rds") 

transit <- read_rds("raw_data/buildings_transit.rds") %>%
  # There were a couple of duplicates;
  # let's take the smallest distance to compensate
  group_by(building_id) %>%
  summarize(train_dist = min(train_dist, na.rm = TRUE),
            bus_dist = min(bus_dist, na.rm = TRUE)) %>%
  ungroup()


read_rds("raw_data/buffer_dist.rds") %>%
  # There are 275 builings in Boston that are unlabelled. We will just remove them.
  filter(nchar(building_id) > 1) %>%
  # Collect the following variables, telling us...
  # This building in THAT CELL was a median of this far away from social infrastructure sites of TYPE X within THAT RADIUS?
  select(cell_id = buffer_id, building_id, type, dist = dist1000) %>%
  mutate(type = type %>% recode_factor(
    "Community Spaces" = "community",
    "Places of Worship" = "worship",
    "Social Businesses" = "social", 
    "Parks" = "parks")) %>%
  pivot_wider(id_cols = c(building_id, cell_id), names_from = type, values_from = dist)  %>%
  # Some buildings got caught in multiple grid cells, presumably because they site at the cross between several lines
  # # eg. building_id == "Bos_0104067000_B0" is in Cell 184, CEll 185, Cell 195, and Cell 196's buffer.
  # However, THANK GOODNESS, their distance measurements are all the same.
  # so, we're just going to consolidate them into unique records, 
  group_by(building_id) %>%
  summarize(
    # Get the modal cell,
    cell_id = cell_id %>% table() %>% sort(decreasing = TRUE) %>% names() %>% .[1] %>% as.double(),
    # and taking the median of their entries
    community = median(community, na.rm = TRUE),
    parks = median(parks, na.rm = TRUE),
    worship = median(worship, na.rm = TRUE),
    social = median(social, na.rm = TRUE)) %>%
  ungroup()  %>%
  left_join(by = "cell_id", y = grid %>%
              select(cell_id, neighborhood, pop_density_int, pop_white, pop_black, pop_hisplat, pop_asian,
                     pop_some_college, median_income, income_inequality, median_monthly_housing_cost)) %>%
  # Get the unique building code.
  # Some buildings have multiple entries, as a B0 & B1 (It only happens rarely, so this describes places that have extensions large enough to render themselves their own polygons.)
  mutate(id = str_extract(building_id, pattern = "[0-9]+")) %>%
  # Now join in Property Records
  left_join(by = c("id" = "pid"), y = dat) %>% 
  # 89999
  # Now join in distance to public transit
  left_join(by = "building_id", y = transit) %>% 
  # Save to file
  saveRDS("building_dist_dataset.rds")


read_rds("building_dist_dataset.rds") %>%
  # Filter to sites within boston neighborhoods
  filter(neighborhood != "Other") %>%
  # Save to file
  saveRDS("building_dist_dataset.rds")
rm(list=ls())




# 5. Boston: Nearest Social Infrastructure
library(dplyr)
library(sf)
library(readr)
library(stringr)
# Next, let's amp this up to estimate our quantities of interest, but for *every* city block. Should be cool!

# Here's how you might visualize these all together
sites <- read_rds("raw_data/sites.rds") 
buildings <- read_rds("raw_data/buildings_points.rds") %>%
  select(building_id, geometry) %>% 
  st_join(read_rds("raw_data/grid.rds") %>% select(cell_id, geometry), left = FALSE)

# Albers Equal Area Conic Projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# We'll use the same function to get the nearest type of social infrastructure, with ```st_nearest_feature()```.
# Identify which sites are closest to each building site
buildings %>% 
  st_join(sites %>% select(type), join = st_nearest_feature) %>%
  mutate(cell_id = str_remove(cell_id, "Cell ") %>% as.numeric()) %>%
  saveRDS("raw_data/nearesttype.rds")

# To reduce the total load on our computers,
# let's get rid of the original buidings dataset
remove(buildings)


# Nearest type now contains a record of every building in Boston, by grid cells, as well as the type of social infrastructure is is nearest to.
read_rds("raw_data/nearesttype.rds") %>% head()





# Let's calculate the total buildings that are nearest to each type of social infrastructure,
# In EACH CELL!
mycount <- read_rds("raw_data/nearesttype.rds") %>%
  as_tibble() %>%
  group_by(cell_id, type) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(cell_id) %>%
  mutate(total = sum(count))

grid = read_rds("raw_data/grid.rds") %>%
  filter(milestone_id %in% c(1:4))  %>%
  # Cut the harbor islands
  filter(!cell_id %in% c(203, 192, 181, 171))

  #filter(!zone %in% c("outside_of_boston", "excluded_boston"))

# Now, let's join that into our grid,
grid %>%
  as_tibble() %>%
  # Repeat the grid four types, once per type shown below
  expand_grid(type = c("Community Spaces", "Places of Worship", "Social Businesses", "Parks"))  %>%
  left_join(by = c("cell_id", "type"), y = mycount) %>%
  # Get nice percentages for each grid cell
  mutate(percent = count / total * 100) %>%
  select(cell_id, type, count, percent, total, pop_density_int, geometry) %>%
  # If any of these weren't filled in, give it a score of zero (since no buildings)
  mutate_at(vars(count, percent, total), list(~if_else(is.na(.), 0, as.numeric(.) ))) %>%
  # convert back to sf format for mapping
  st_as_sf(crs = 4326) %>%
  mutate(type = type %>% factor(levels = c(
    "Community Spaces", "Places of Worship", "Social Businesses", "Parks"))) %>%
  saveRDS("raw_data/mygridpercent.rds")





##########################################################
# CASE STUDY VISUALS #####################################
rm(list = ls())

cells = c(78, 162, 97)


#ggplot() +
#  geom_sf(data = read_rds("raw_data/streets.rds")) +
#  geom_sf(data= read_rds("raw_data/grid.rds") %>% filter(cell_id == 162)) 

# Here's how you might visualize these all together
grid <- read_rds("raw_data/grid.rds") %>% filter(cell_id %in% cells)

streets <- read_rds("raw_data/streets.rds") %>%
  st_crop(grid)

###Creating vector for color of ridgeplots###
mycolors <- viridis(n = 4, option = "plasma")
mycolors[4] <- viridis(n = 4, option = "plasma", end = 0.9)[4]

nearest <- read_rds("raw_data/nearesttype.rds") %>% filter(cell_id %in% cells)

# Albers Equal Area Conic Projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

mydist <- read_rds("building_dist_dataset.rds") %>%
  # Filter to sites within boston neighborhoods
  filter(neighborhood != "Other") %>%
  select(building_id, cell_id, community:social) %>%
  pivot_longer(cols = -c(building_id, cell_id), names_to = "type", values_to = "dist") %>%
  mutate(type = type %>% recode_factor(
    "parks" = "Parks",
    "social" = "Social\nBusinesses",
    "worship" = "Places of\nWorship",
    "community" = "Community\nSpaces"))  %>%
  filter(cell_id %in% cells)

save(grid, streets, mycolors, nearest, aea, mydist, file = "raw_data/case_studies.rdata")



