#this script cleans occupancy data
require(tidyr)
require(dplyr)

#load csv of semi-cleaned data

make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x=="NA"; x} else {
    x}

options(scipen = 999) #just because Percent_Occupancy_fix is really annoying when it's in scientific notation


occupancy <- read_csv("../data/Occupany.csv") %>%
  mutate(abbrev = str_extract(abrev,"[:upper:]{2,}")) %>%
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
  mutate(PercCap = case_when(PercCap == "-" ~ (FelOth/DesCap)*100,
                             is.na(PercCap) ~ (FelOth/DesCap)*100,
                             TRUE ~ as.numeric(.$PercCap))) %>%
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
  mutate(Prison_or_Jail_in_2020 = case_when((str_detect(abbrev, "PDC") | str_detect(abbrev, "RIO") | str_detect(abbrev, "SRTA") | str_detect(abbrev, "SBRN") | str_detect(abbrev, "FRCC")) ~ "county_jail",
                        TRUE ~ "state_prison")) %>%
  mutate(percent_bin = cut(occupancy$Percent_occupancy_fix, 
                           breaks = c(0, 90, 100, 125, 150, 200, 250, 300, Inf))) # makes a discrete set of values to assign colors for plotting

#filter(., my_col %in% c(“Total”, “Female”, “Male”)

#Make a lebeled vector for color binning to pair with the "percent_bin" column
colors_percent <- c("blue3", "yellow", "indianred", "tomato3", "orangered3", "red3", "red2", "red") #create a paletta that's the same length as your binning
names(colors_percent) <- levels(occupancy$percent_bin)


#saveRDS(occupancy, "../data/occupancy_cleaned.RDS")
