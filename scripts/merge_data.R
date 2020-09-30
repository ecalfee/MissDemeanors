#read in both RDS files
occupancy <- readRDS("../data/occupancy_cleaned.RDS")
lat_long <- readRDS("../data/prisons_lat_long_values.RDS")

#add occupancy data and lat/long data
full_data <- full_join(lat_long, occupancy, by = "abbrev")

#check to make sure it worked
full_data

#if you want
saveRDS(full_data, "../data/full_data.RDS")
