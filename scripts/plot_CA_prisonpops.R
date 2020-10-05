# plotting the map with points on it
require(dplyr)
require(ggplot2)
require(maps)
require(here) # paths relative to project directory "MissDemeanors/"

# load data to map
map_data <- readRDS(here("data/full_data.RDS")) %>%
  mutate(Year = as.integer(Year))
CA_polygon = ggplot2::map_data("state")  %>% # united states data
  filter(., region == "california") 

plot_map <- function(set_year, state_polygon, data){
  # filter to current year
  data_yr <- data %>% dplyr::filter(Year == set_year)

  ggplot() + geom_polygon(data = state_polygon, 
                          aes(x = long, y = lat, group = group), 
                          alpha = 0.25) + # plot of California
  geom_point(data = data_yr, 
             aes(x = long, y = lat, fill = percent_bin, 
                 size = Design_capacity, color = percent_bin), 
             alpha = 0.75, shape = 21) + # add bubbles for each prison
  scale_radius(name = "Prison Size\n(# inmates at capacity)",
                    breaks = c(1000, 3000, 5000),
                    limits = c(0, 6000),
                    range = c(3, 10),
                    labels = c(1000, 3000, 5000)) + 
  scale_fill_viridis_d(option = "inferno", direction = -1, drop = FALSE, 
                       name="Percent Occupancy",
                       labels=c("0-90%", "90-100%", "100-125%", "125-150%", "150-200%", "200-250%", "250-300%")) +
  scale_color_viridis_d(option = "inferno", direction = -1, drop = FALSE) + #color bubbles
  guides(fill = guide_legend(order=1, 
                             override.aes = list(alpha = 0.8, size = 7)),
         size = guide_legend(order=2),
         color = FALSE) + # make sure that legends are in a consistent order

  annotate("text", label = paste0("Total Prison Population: ", 
                                  sum(data_yr %>% 
                                        dplyr::select(Inmates), na.rm = T)), 
           x = -115, y = 40.0, 
           size = 5, colour = "black") +  #add total prison inmate population text
  geom_text() +
  xlim(c(-124.5, -111)) +
  annotate("text", label = paste0("Total Prison Occupancy: ", 
                                  data_yr %>% # % space occupied is (total inmates + civil additions)/(total design capacity)
                                   dplyr::summarise(tot = 100*sum(Total)/sum(Design_capacity)) %>% 
                                   round(., digits = 1) %>%
                                   unlist(.), "%"), 
    x = -115, y = 41.0, size = 7, 
    # text is red if over 137.5% court limit set in Brown v. Plata 2011, black otherwise
    colour = ifelse(100*sum(data_yr$Total)/sum(data_yr$Design_capacity) >= 137.5, "red", "black")) + #add mean occupancy text
    theme_void() +  #turn back to #theme_classic if you want axes on
    theme(legend.title = element_text(size = 16),
          legend.text = element_text(size = 14))
}



# test by plotting 1 year (2020)
# plot_map(set_year = 2006, state_polygon = CA_polygon, data = map_data)
