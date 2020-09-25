#Get lat/long data from addresses
#Katherine Corn and Erin Calfee
#25 September 2020
require(rvest)
require(tidyverse)
require(xml2)

webpage <- xml2::read_html("https://www.cdcr.ca.gov/facility-locator/adult-institutions")

tbls_ls <- webpage %>%
  rvest::html_nodes("table") %>%
  .[[1]] %>% #tell us which tables on the page we want to pull out -- fortunately for us there's only one on this specific website and so we can just pull the first one -- you want to use [[]] because we only need one of the tables. if you wanted 2 tables you'd pull [] so you would end up with a list of tables. useful but not waht we need here
  rvest::html_table(header = T, trim = T, fill = TRUE) #this is the command that parses the html into data frame

str(tbls_ls)


address_list <- tbls_ls %>%
  dplyr::summarize(across(.cols = everything(), ~ str_replace_all(., pattern="\n", replacement = " ")))
colnames(address_list) <- c("Facility","Address","Mailing_address")

#geocode
require(tidygeocoder)

lat_long <- address_list %>%
  mutate_at("Address", ~ str_sub(., end=-15)) %>%
  geocode(address_list, address = Address)


test <- data.frame(t(data.frame(c("test name", "100 Prison Road, Represa, CA 95671"))))

geocode(test,X2)

