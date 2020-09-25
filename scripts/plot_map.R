require(ggplot2)
require(dplyr)
# this script plots a map of california
# with all the prison facilities

# data for dates facilities opened:
# https://web.archive.org/web/20071214123130/http://www.cdcr.ca.gov/Visitors/docs/20071015-WEBmapbooklet.pdf

california <- ggplot2::map_data("state")  %>% # united states data
  filter(., region == "california")

# script to plot map of California
california %>%
  ggplot(., aes(x = long, y = lat, group = group)) +
  geom_polygon() + # plot California as a polygon
  theme_classic()
  