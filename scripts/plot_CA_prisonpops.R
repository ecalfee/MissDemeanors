#plot population by lat/long
require(tidyverse)
require(ggmap)
require(tidygeocoder)

#load csv of semi-cleaned data
pop_sizes <- read_csv("~/Desktop/git_corn/MissDemeanors/data/CA_prison_pop_reports/july_2020_popn.csv") %>%
  rename(Facility = Institution)
pop_sizes <- read_csv("../data/CA_prison_pop_reports/july_2020_popn.csv") %>%
  rename(Facility = Institution)

#
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
  tidygeocoder::geocode(address_list, address = Address)

full_data <- left_join(lat_long,pop_sizes, id = Facility) #lost 6 institutions here that we have pop size data for but no address

#plot the map
california <- ggplot2::map_data("state")  %>% # united states data
  filter(., region == "california")

# script to plot map of California
california %>%
  ggplot(., aes(x = long, y = lat, group = group)) +
  geom_polygon() + # plot California as a polygon
  theme_classic()


# plotting the map with some points on it
ggmap(california) +
  geom_point(data = full_data, aes(x = long, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

ggplot() + geom_polygon(data = ggplot2::map_data("state")  %>% # united states data
                          filter(., region == "california"), aes(x = long, y = lat, group = group)) + geom_point(data = full_data, aes(x = long, y = lat, fill = Percent_Occupied, alpha = 1), size = 5, shape = 21) + theme_classic() + scale_fill_gradient(low="blue", high="red")


