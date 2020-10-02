require(dplyr)
require(tidyr)
require(here) # paths relative to project directory "MissDemeanors/"

#read in both RDS files
occupancy <- readRDS(here("data/occupancy_cleaned.RDS"))
lat_long <- readRDS(here("data/prisons_lat_long_values.RDS"))

#add occupancy data and lat/long data
full_data <- full_join(occupancy, lat_long, by = "abbrev")

#check to make sure it worked
full_data

#if you want
saveRDS(full_data, here("data/full_data.RDS"))
