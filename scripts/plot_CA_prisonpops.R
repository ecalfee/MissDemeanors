#plot population by lat/long
require(tidyverse)

#load csv of semi-cleaned data

make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}

options(scipen = 999) #just because Percent_Occupancy_fix is really annoying when it's in scientific notation


occupancy <- read_csv("../data/Occupany.csv") %>%
  mutate(abbrev = str_extract(abrev,"[:upper:]{3,}")) %>%
  mutate(abbrev = case_when(abbrev == "FOL" ~ "FSP",
                            abbrev == "FWF" ~ "FSP",
                            abbrev == "HDP" ~ "HDSP",
                            abbrev == "VSPM" ~ "VSP",
                            abbrev == "IRON" ~ "ISP",
                            abbrev == "FEMALE" ~ "NA",
                            abbrev == "MALE" ~ "NA",
                            abrev == "Female" ~ "NA",
                            abrev == "Male" ~ "NA",
                            abrev == "Total" ~ "NA",
                            TRUE ~ as.character(.$abbrev))) %>%
  mutate(PercCap = case_when(is.na(PercCap) ~ (FelOth/DesCap)*100,
                                 TRUE ~ .$PercCap)) %>%
  dplyr::mutate(Percent_occupancy_fix = (FelOth/DesCap)*100)  %>% #if you look, some of the provided PercCaps don't match the ratio of inmates to design capacity
  mutate(Year = str_extract(Year,"[:digit:]{4}")) %>%
  mutate(name = case_when((abbrev == "VSP" & Year < 2013) ~ "VALLEY SP WOMEN",
                           TRUE ~ as.character(.$name))) %>%
  mutate_at(vars(abbrev), ~ make.true.NA(.)) %>%
  dplyr::filter(complete.cases(abbrev)) %>% #IMPORTANT to make sure we get rid of columns that are a total/sum
  rename(Facility = name, Inmates = `FelOth`, Percent_capacity_provided = PercCap, Design_capacity = DesCap, Staff_capacity = StafCap, Civil_additions = CivAdd) %>%
    group_by(Year, abbrev) %>%
    dplyr::mutate(Sex = case_when((str_detect(Facility, "WOMEN") | str_detect(Facility, "FEMALE") | str_detect(Facility, "Women") | str_detect(Facility, "Female")| str_detect(abbrev, "WF") | (n() == 2 ) & (rank(Yindex) == 2)) ~ "women",
                           TRUE ~ "men")) %>%
  ungroup() %>%
  dplyr::select(everything(), - X1, -abrev, -Yindex) %>%
  mutate(Prison_or_Jail_in_2020 = case_when((str_detect(abbrev, "PDC") | str_detect(abbrev, "RIO") | str_detect(abbrev, "SRTA") | str_detect(abbrev, "SBRN") | str_detect(abbrev, "FRCC"))
                                    ~ "county_jail",
                                    TRUE ~ "state_prison")) 

#filter(., my_col %in% c(“Total”, “Female”, “Male”)
occupancy$percent_bin <- cut(occupancy$Percent_occupancy_fix, # makes a discrete set of values to assign colors for plotting
                             breaks = c(0, 50, 80, 90, 100, 150, 200, 300, Inf))
colors_percent <- c("blue", "green", "yellow", "orange", "orangered3", "red3", "red2", "red", "purple") #create a paletta that's the same length as your binning
names(colors_percent) <- levels(occupancy$percent_bin)

  # mutate(color = case_when(Percent_occupancy_fix >= 2000 ~ "red",
  #        2000 > Percent_occupancy_fix & Percent_occupancy_fix >= 1000 ~ "red2",
  #        1000 > Percent_occupancy_fix & Percent_occupancy_fix >= 500 ~ "red3",
  #        500 > Percent_occupancy_fix & Percent_occupancy_fix >= 300 ~ "orangered",
  #        300 > Percent_occupancy_fix & Percent_occupancy_fix >= 200 ~ "orangered3",
  #        200 > Percent_occupancy_fix & Percent_occupancy_fix >= 150 ~ "tomato3",
  #        150 > Percent_occupancy_fix & Percent_occupancy_fix >= 125 ~ "indianred3",
  #        125 > Percent_occupancy_fix & Percent_occupancy_fix >= 110 ~ "mediumorchid1",
  #        110 > Percent_occupancy_fix & Percent_occupancy_fix >= 100 ~ "purple",
  #        100 > Percent_occupancy_fix & Percent_occupancy_fix >= 90 ~ "purple3",
  #        90 > Percent_occupancy_fix & Percent_occupancy_fix >= 80 ~ "blue2",
  #        80 > Percent_occupancy_fix & Percent_occupancy_fix >= 60 ~ "blue3",
  #        60 > Percent_occupancy_fix & Percent_occupancy_fix >= 40 ~ "navy",
  #        40 > Percent_occupancy_fix & Percent_occupancy_fix >= 0 ~ "midnightblue"))
  # 

lat_long <- readRDS("../data/prisons_lat_long_values.RDS")

#add occupancy data and lat/long data
full_data <- full_join(lat_long, occupancy, by = "abbrev")

# plotting the map with  points on it

set_year = 2020 #set the year you want here

ggplot() + geom_polygon(data = ggplot2::map_data("state")  %>% # united states data
                          filter(., region == "california"), aes(x = long, y = lat, group = group), alpha = 0.25) + #plot of California
  geom_point(data = full_data %>% dplyr::filter(Year == set_year), aes(x = long, y = lat, fill = percent_bin, size = Inmates), alpha = 0.8, shape = 21) + #add bubbles
  theme_classic() + 
  scale_color_discrete(values = colors_percent) +
  #scale_fill_gradient(low="blue", high="red") + 
  #scale_colour_gradientn(colours = myPalette(100), limits=c(0, 3600)) +
  geom_text() +
  annotate("text", label = paste0("Total Prison Population: ",sum(full_data %>% filter(Year == set_year) %>% dplyr::select(Inmates), na.rm = T)), x = -117.0, y = 40.0, size = 3, colour = "black") + 
  geom_text() +
  annotate("text", label = paste0("Average prison occupancy: ", round(mean(as.numeric(unlist(full_data %>% filter(Year == set_year) %>% dplyr::select(Percent_occupancy_fix)), na.rm = T), , digits = 5)) ,"%"), x = -117.0, y = 41.0, size = 4, colour = "black") 

