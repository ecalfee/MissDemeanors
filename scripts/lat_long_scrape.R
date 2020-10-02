# Get lat/long data from addresses to plot prison locations
require(rvest)
require(tidyr)
require(dplyr)
require(stringr)
require(xml2)
require(tidygeocoder) #geocode
require(here) # paths relative to project directory "MissDemeanors/"

# scrape prison addresses from CA Dept. of Corrections and Rehabilitation Website
webpage <- xml2::read_html("https://www.cdcr.ca.gov/facility-locator/adult-institutions")

tablescrape <- webpage %>%
  rvest::html_nodes("table") %>%
  .[[1]] %>% #tell us which tables on the page we want to pull out -- fortunately for us there's only one on this specific website and so we can just pull the first one -- you want to use [[]] because we only need one of the tables. if you wanted 2 tables you'd pull [] so you would end up with a list of tables. useful but not what we need here
  rvest::html_table(header = T, trim = T, fill = TRUE) # this is the command that parses the html into data frame

#str(tbl)
colnames(tablescrape) <- c("Facility","Address","Mailing_address")

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

missing_addresses <- tibble(.rows = 6)
missing_addresses$Facility <- c("Northern California Women's Facility (NCWF)","Pitchess Detention Center North Facility (PDC)","Santa Rita Jail (SRTA)","Rio Cosumnes Correctional Center (RIO)", "San Francisco County Jail #5 (SBRN)","Female Rehabilitation Community Correctional Center Bakersfield (FRCC)")
missing_addresses$Address <- c("7150 Arch Road, Stockton, CA 95213", "29320 The Old Rd Castaic CA 91384", "5325 Broder Blvd., Dublin, CA 94568", "12500 Bruceville Road, Elk Grove, CA 95757","#1 Moreland Drive, San Bruno, CA 94066","Bakersfield, CA, 93301")
missing_addresses$Mailing_address <- c("Northern California Women's Facility","Pitchess Detention Center North Facility","Santa Rita Jail", "Rio Cosumnes Correctional Center","County Jail #5", "Female Rehabilitation Community Correctional Center")

# get addresses
clean_scrape <- tablescrape %>%
  dplyr::mutate_all(., ~str_replace_all(., pattern="\n", replacement = " ")) %>%
  dplyr::mutate(Address = sapply(Address, clean_address))

full_address_list <- bind_rows(clean_scrape,missing_addresses) %>%
  dplyr::mutate(Zip = str_extract(Address, "[:graph:]{5,}$")) %>%
  mutate(abbrev = str_extract(Facility, "[:upper:]{2,}"))

lat_long <- full_address_list %>%
  tidygeocoder::geocode(.,address = Address, method = "cascade") %>%
  dplyr::rename(lat_specific = lat, long_specific = long) %>%
  tidygeocoder::geocode(., address = Zip, method = "osm") %>%
  dplyr::mutate(lat = dplyr::coalesce(lat_specific,lat)) %>% #smushes together latitude columns, replacing NAs in lat_specific with lat computed using Zip
  dplyr::mutate(long = dplyr::coalesce(long_specific,long)) %>% 
  dplyr::select(everything(), -Zip, - lat_specific, -long_specific, - geo_method)

# A couple of fixes after a visual check:
# these lat/lon coordinates from geocode were clearly wrong
# and we looked them up by hand on google maps
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

saveRDS(lat_long, here("data/prisons_lat_long_values.RDS"))
