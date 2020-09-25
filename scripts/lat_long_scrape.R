#Get lat/long data from addresses
#Katherine Corn and Erin Calfee
#25 September 2020
require(rvest)
require(tidyr)
require(dplyr)
require(stringr)
require(xml2)
require(tidygeocoder) #geocode
# currently not used
#library(ggplot2)
#library(ggmap)
#library(tmaptools)

webpage <- xml2::read_html("https://www.cdcr.ca.gov/facility-locator/adult-institutions")

tbl <- webpage %>%
  rvest::html_nodes("table") %>%
  .[[1]] %>% #tell us which tables on the page we want to pull out -- fortunately for us there's only one on this specific website and so we can just pull the first one -- you want to use [[]] because we only need one of the tables. if you wanted 2 tables you'd pull [] so you would end up with a list of tables. useful but not waht we need here
  rvest::html_table(header = T, trim = T, fill = TRUE) # this is the command that parses the html into data frame

#str(tbl)
colnames(tbl) <- c("Facility","Address","Mailing_address")

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
address_list <- tbl %>%
  dplyr::mutate_all(., ~str_replace_all(., pattern="\n", replacement = " ")) %>%
  dplyr::mutate(Address = sapply(Address, clean_address))

lat_long <- address_list %>%
  geocode(address_list, address = Address)