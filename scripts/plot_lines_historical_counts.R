# this script makes a line plot of historical prison
# population sizes through time
require(dplyr)
require(ggplot2)
require(here) # paths relative to project directory "MissDemeanors/"

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


# plot
# ggplot(prison_pop, aes(x = year, y = inmate_count)) +
#   geom_line() +
#   theme_light() +
#   ylab("Total inmates in CA prisons")
# ggplot(prison_pop, aes(x = year, y = pop_estimate/10^6)) +
#   geom_line() +
#   theme_light() +
#   ylab("Total CA residents (millions)")

plot_lines <- function(d = prison_pop, e = events, set_year){
  ggplot(d, aes(x = year, y = inmate_per_100k)) +
  geom_line() +
  theme_classic() +
  ylab("Inmates per 100,000 residents") +
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
  scale_color_manual(values = c("chocolate3", "purple4"))
}
#plot_lines(set_year = 2020) 

# write out the names of all the legislation
plot_events <- function(e = events, set_year){
  e %>%
    mutate(index= nrow(.):1,
           text = paste(year, name, short_name)) %>%
    ggplot(., aes(x = 1, y = index, 
                  color = effect,
                  label = text,
                  fill = (year == set_year)), # highlight if legislation is current year
           x = 1,# all in same column
           size = 0.1) +
    geom_label() +
    theme_void() +
    scale_color_manual(values = c("chocolate3", "purple4")) +
    scale_fill_manual(values = c("white", alpha("yellow1", 0.4))) + # highlight same color as map background
    guides(color = F,
           fill = F)
}
#plot_events(set_year = 2020)  
