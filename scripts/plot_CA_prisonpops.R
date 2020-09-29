#plot population by lat/long
require(tidyverse)
require(tidygeocoder)

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
  dplyr::mutate(Percent_Occupancy_fix = (FelOth/DesCap)*100)  %>% #if you look, some of the provided PercCaps don't match the ratio of inmates to design capacity
  mutate(Year = str_extract(Year,"[:digit:]{4}")) %>%
  mutate(name = case_when((abbrev == "VSP" & Year < 2013) ~ "VALLEY SP WOMEN",
                           TRUE ~ as.character(.$name))) %>%
  mutate_at(vars(abbrev), ~ make.true.NA(.)) %>%
  dplyr::filter(complete.cases(abbrev)) %>% #IMPORTANT to make sure we get rid of columns that are a total/sum
  rename(Facility = name, Inmates = `FelOth`, Percent_capacity_provided = PercCap, Design_capacity = DesCap, Staff_capacity = StafCap, Civil_additions = CivAdd) %>%
    group_by(Year, abbrev) %>%
    dplyr::mutate(Sex = case_when((str_detect(Facility, "WOMEN") | str_detect(Facility, "FEMALE") | str_detect(Facility, "Women") | str_detect(Facility, "Female")| str_detect(abbrev, "WF") | (n() == 2 ) & (rank(Yindex) == 2))
                           ~ "women",
                           TRUE ~ "men")) %>%
  ungroup() %>%
  dplyr::select(everything(), - X1, -abrev) %>%
  mutate(Prison_or_Jail_in_2020 = case_when((str_detect(abbrev, "PDC") | str_detect(abbrev, "RIO") | str_detect(abbrev, "SRTA") | str_detect(abbrev, "SBRN") | str_detect(abbrev, "FRCC"))
                                    ~ "county_jail",
                                    TRUE ~ "state_prison"))


#
require(rvest)
require(tidyverse)
require(xml2)

webpage <- xml2::read_html("https://www.cdcr.ca.gov/facility-locator/adult-institutions")

tablescrape <- webpage %>%
  rvest::html_nodes("table") %>%
  .[[1]] %>% #tell us which tables on the page we want to pull out -- fortunately for us there's only one on this specific website and so we can just pull the first one -- you want to use [[]] because we only need one of the tables. if you wanted 2 tables you'd pull [] so you would end up with a list of tables. useful but not what we need here
  rvest::html_table(header = T, trim = T, fill = TRUE) # this is the command that parses the html into data frame

#str(tablescrape)
colnames(tablescrape) <- c("Facility","Address","Mailing_address")

missing_addresses <- tibble(.rows = 6)
missing_addresses$Facility <- c("Northern California Women's Facility (NCWF)","Pitchess Detention Center North Facility (PDC)","Santa Rita Jail (SRTA)","Rio Cosumnes Correctional Center (RIO)", "San Francisco County Jail #5 (SBRN)","Female Rehabilitation Community Correctional Center Bakersfield (FRCC)")
missing_addresses$Address <- c("7150 Arch Road, Stockton, CA 95213", "29320 The Old Rd Castaic CA 91384", "5325 Broder Blvd., Dublin, CA 94568", "12500 Bruceville Road, Elk Grove, CA 95757","#1 Moreland Drive, San Bruno, CA 94066","Bakersfield, CA, 93301")
missing_addresses$Mailing_address <- c("Northern California Women's Facility","Pitchess Detention Center North Facility","Santa Rita Jail", "Rio Cosumnes Correctional Center","County Jail #5", "Female Rehabilitation Community Correctional Center")

# function takes in a raw address and cleans it up
# e.g. by removing extra parenthesis/symbols and phone numbers
clean_address <- function(a){ 
  begin_parenthesis <- stringr::str_locate(a, "\\(")[1, "start"] # beginning of phone number
  address <- stringr::str_sub(a, 1, begin_parenthesis - 1) %>% # remove phone number
    stringr::str_replace(., "\\#", "") %>% # remove special character pound symbol
    stringr::str_replace(., "\\* ", "") %>%
    stringr::str_trim(., "both") # trim any white space
  return(address)
}



# get addresses
clean_scrape <- tablescrape %>%
  dplyr::mutate_all(., ~str_replace_all(., pattern="\n", replacement = " ")) %>%
  dplyr::mutate(Address = sapply(Address, clean_address))
  
full_address_list <- bind_rows(clean_scrape,missing_addresses) %>%
  dplyr::mutate(Zip = str_extract(Address, "[:graph:]{5,}$")) %>%
  mutate(abbrev = str_extract(Facility, "[:upper:]{2,}"))

lat_long <- full_address_list %>%
  tidygeocoder::geocode(address_list,address = Address, method = "cascade") %>%
  dplyr::rename(lat_specific = lat, long_specific = long) %>%
  tidygeocoder::geocode(address_list, address = Zip, method = "osm") %>%
  dplyr::mutate(lat = dplyr::coalesce(lat_specific,lat)) %>%
  dplyr::mutate(long = dplyr::coalesce(long_specific,long)) %>%
  dplyr::select(everything(), -Zip, - lat_specific, -long_specific, - geo_method)


#A couple of fixes
lat_long[lat_long$abbrev=="CCC", "lat"] <- 40.397402
lat_long[lat_long$abbrev=="CCC", "long"] <- -120.511749
lat_long[lat_long$abbrev=="CCI", "lat"] <- 35.113301
lat_long[lat_long$abbrev=="CCI", "long"] <- -118.5694837
lat_long[lat_long$abbrev=="CMC", "lat"] <- 35.272023
lat_long[lat_long$abbrev=="CMC", "long"] <- -120.672099
lat_long[lat_long$abbrev=="HDSP", "lat"] <- 40.40527
lat_long[lat_long$abbrev=="HDSP", "long"] <- -120.526065
lat_long[lat_long$abbrev=="MCSP", "lat"] <- 38.370631
lat_long[lat_long$abbrev=="MCSP", "long"] <- -120.953728


#add occupancy data and lat/long data
full_data <- full_join(lat_long, occupancy, by = "abbrev")

# plotting the map with  points on it

set_year = 2020 #set the year you want here

ggplot() + geom_polygon(data = ggplot2::map_data("state")  %>% # united states data
                          filter(., region == "california"), aes(x = long, y = lat, group = group), alpha = 0.25) + #plot of California
  geom_point(data = full_data %>% dplyr::filter(Year == set_year), aes(x = long, y = lat, fill = Percent_Occupancy_fix, size = Inmates), alpha = 0.8, shape = 21) + #add bubbles
  theme_classic() + 
  scale_fill_gradient(low="blue", high="red") + 
  #scale_colour_gradientn(colours = myPalette(100), limits=c(0, 3600)) +
  geom_text() +
  annotate("text", label = paste0("Total Prison Population: ",sum(full_data %>% filter(Year == set_year) %>% dplyr::select(Inmates), na.rm = T)), x = -117.0, y = 40.0, size = 3, colour = "black") + 
  geom_text() +
  annotate("text", label = paste0("Average prison occupancy: ", round(mean(as.numeric(unlist(full_data %>% filter(Year == set_year) %>% dplyr::select(Percent_Occupancy_fix)), na.rm = T), , digits = 5)) ,"%"), x = -117.0, y = 41.0, size = 4, colour = "black") 

