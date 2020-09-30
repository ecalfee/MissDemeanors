# plotting the map with points on it
require(dplyr)
require(ggplot2)

full_data <- readRDS("../data/full_data.RDS")

plot_map <- function(set_year, data){

  #colors_percent <- c("blue3", "yellow", "indianred", "tomato3", "orangered3", "red3", "red2", "red") #create a paletta that's the same length as the binning
  #names(colors_percent) <- levels(data$percent_bin)
  
  jitter <- position_jitter(width = 0.15, height = 0.15, seed = 100)
  
ggplot() + geom_polygon(data = ggplot2::map_data("state")  %>% # united states data
                          filter(., region == "california"), aes(x = long, y = lat, group = group), alpha = 0.25) + #plot of California
  geom_point(data = data %>% dplyr::filter(Year == set_year), aes(x = long, y = lat, fill = percent_bin, size = Inmates, color = percent_bin), alpha = 0.75, shape = 21, position = jitter) + #add bubbles
  scale_size_binned(name = "Number of\nInmates",
                    breaks = c(1000,3000,5000),
                    limits = c(0,6000),
                    labels = c(1000,3000,5000)) + 
  scale_fill_viridis_d(option = "inferno", direction = -1, drop = FALSE, 
                       name="Percent\nOccupancy",
                       labels=c("0-90%", "90-100%", "100-125%", "125-150%", "150-200%", "200-250%", "250-300%")) +
  scale_color_viridis_d(option = "inferno", direction = -1, drop = FALSE) + #color bubbles
  guides(fill = guide_legend(order=1, override.aes = list(alpha = 0.85)),
         size = guide_legend(order=2),
         color = FALSE) + #make sure that legends are in a consistent order
  geom_text() +
  annotate("text", label = paste("Year:", set_year), x = -117.0, y = 42.0, size = 4, colour = "black") + #add year label
  geom_text() +
  annotate("text", label = paste0("Total Prison Population: ",sum(data %>% filter(Year == set_year) %>% dplyr::select(Inmates), na.rm = T)), x = -117.0, y = 40.0, size = 3, colour = "black") +  #add total prison inmate population text
  geom_text() +
  annotate("text", label = paste0("Total prison occupancy: ", round(mean(as.numeric(unlist(data %>% filter(Year == set_year) %>% dplyr::select(Percent_occupancy))), na.rm = T), digits = 2) ,"%"), x = -117.0, y = 41.0, size = 4, colour = "black") + #add mean occupancy text
    theme_void()  #turn back to #theme_classic if you want axes on

}

plot_map(set_year = 2020, data = full_data)
