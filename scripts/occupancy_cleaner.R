#this script cleans occupancy data
require(tidyr)
require(dplyr)
require(readr)
require(stringr)

#load csv of semi-cleaned data

occupancy <- read_csv("../data/Occupany.csv") %>%
  mutate(abbrev = str_extract(abrev,"[:upper:]{2,}")) %>%
  mutate(abbrev = case_when(abbrev == "FOL" ~ "FSP",
                            abbrev == "FWF" ~ "FSP",
                            abbrev == "HDP" ~ "HDSP",
                            abbrev == "VSPM" ~ "VSP",
                            abbrev == "IRON" ~ "ISP",
                            TRUE ~ as.character(.$abbrev))) %>%
  dplyr::filter(complete.cases(abbrev)) %>% 
  filter(., !abbrev %in% c("Total", "Female", "Male", "Total", "NA","FEMALE","MALE","TOTAL")) %>% #take out rows that are sums by group
  mutate(DesCap = as.numeric(case_when(abbrev == "SRTA" ~ "395",
                             abbrev == "PDC" ~ "NA", #no data on this design capacity--replacing the 0 with NA so we can work with it as no info
                             abbrev == "RIO" ~ "NA",
                             DesCap == 0 ~ "NA",
                             TRUE ~ as.character(.$DesCap)))) %>% #inducing NAs by coercion is fine and intentional. some facilities have no info for Percent Capacity or Design Capacity
  mutate(PercCap = case_when(PercCap == "-" ~ (FelOth/DesCap)*100,
                             is.na(PercCap) ~ (FelOth/DesCap)*100,
                             TRUE ~ as.numeric(.$PercCap))) %>% #again, inducing NAs by coercion is fine and intentional
  mutate(Year = str_extract(Year,"[:digit:]{4}")) %>% #get year from filename
  mutate(name = case_when((abbrev == "VSP" & Year < 2013) ~ "VALLEY SP WOMEN",
                          TRUE ~ as.character(.$name))) %>% #this prison changed from being a women's prison to a men's facility in 2013 -- just updating the facility name here to that my prison Sex IDer later will note it correctly
  rename(Facility = name, Inmates = `FelOth`, Percent_occupancy = PercCap, Design_capacity = DesCap, Staff_capacity = StafCap, Civil_additions = CivAdd) %>%
  group_by(Year, abbrev) %>%
  dplyr::mutate(Sex = case_when((str_detect(Facility, "WOMEN") | str_detect(Facility, "FEMALE") | str_detect(Facility, "Women") | str_detect(Facility, "Female")| str_detect(abbrev, "WF") | (n() == 2 ) & (rank(Yindex) == 2)) ~ "women",
                                TRUE ~ "men")) %>%
  ungroup() %>%
  dplyr::select(everything(), - X1, -abrev, -Yindex) %>%
  mutate(Prison_or_Jail_in_2020 = case_when((str_detect(abbrev, "PDC") | str_detect(abbrev, "RIO") | str_detect(abbrev, "SRTA") | str_detect(abbrev, "SBRN") | str_detect(abbrev, "FRCC")) ~ "county_jail",
                        TRUE ~ "state_prison"))  %>% #label mens and womens facilities
  mutate(percent_bin = cut(Percent_occupancy, 
                           breaks = c(0, 90, 100, 125, 150, 200, 250, 300))) # makes a discrete grouping of values to assign colors for plotting


saveRDS(occupancy, "../data/occupancy_cleaned.RDS")
