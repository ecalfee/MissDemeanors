# this script makes a line plot of historical prison
# population sizes through time
require(dplyr)
require(ggplot2)
require(here) # paths relative to project directory "MissDemeanors/"
require(stringr)
require(ggfittext)

# read in data
# prison population totals
prison <- read.table(here("data/total_inmate_pop_by_year.txt"),
                     header = T, sep = "\t", stringsAsFactors = F)

# California population totals
CA_pop <- read.table(here("data/total_CA_pop_by_year.txt"),
                     header = T, sep = "\t", stringsAsFactors = F)

# calculate inmates per 100,000 total population
prison_pop <- left_join(prison, CA_pop, by = "year") %>%
  mutate(inmate_per_100k = inmate_count/pop_estimate*100000)

# historical laws relevant to prison overcrowding
events <- read.table("data/historical_CA_events.csv",
                   header = T, sep = ",", stringsAsFactors = F)

plot_lines <- function(d = prison_pop, e = events, set_year){
  ggplot(d, aes(x = year, y = inmate_per_100k)) +
  geom_line() +
  theme_classic() +
  ylab("Inmates per\n100,000 residents") +
    # add lines for major legislation and other events affecting inmate populations
  geom_vline(data = e, #filter(e, 
                     #name %in% c("AB 109", "Prop 47", "Prop 57")),
             aes(color = effect, # different color for events that increase vs. decrease prison pops
                 xintercept = year)) +
  guides(color = F) +
    # add a point for current year
  geom_point(data = filter(d, year == set_year),
             pch = 20, size = 3) +
  # match map colors -- purples are events that lead to higher # prison inmates
    # and oranges are events that lead to lower # prison inmates
  scale_color_manual(values = c("chocolate3", "purple4")) +
  theme(axis.title = element_text(size = 14))
}
#plot_lines(set_year = 2020) 

# write out the names of all the legislation
effect_colors <- c("chocolate3", "purple4", "white")
names(effect_colors) <- c("-", "+", "in_future") # only show events that have happened (white if in the future still)
plot_events <- function(e = events, set_year){
  e %>%
    mutate(index= nrow(.):1,
           text = paste0(year, ": ", name, " ", short_name),
           effect = ifelse(year > set_year, "in_future", effect)) %>%
    ggplot(., aes(x = 1, y = index, 
                  color = effect,
                  label = text), # highlight if legislation is current year
           x = 1) + # all in same column
    geom_fit_text(place = "topleft", reflow = TRUE) + # left-align all text labels
    theme_void() +
    scale_color_manual(values = effect_colors) +
    guides(color = F) +
    ggtitle("   Major Legislation") +
    theme(legend.position = "bottom",
      plot.title = element_text(size = 16, margin = margin(11, 0, 0, 0)))
}
#plot_events(set_year = 2000)
