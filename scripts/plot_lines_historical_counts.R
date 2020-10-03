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
ggplot(prison_pop, aes(x = year, y = inmate_count)) +
  geom_line() +
  theme_light() +
  ylab("Total inmates in CA prisons")
ggplot(prison_pop, aes(x = year, y = pop_estimate/10^6)) +
  geom_line() +
  theme_light() +
  ylab("Total CA residents (millions)")

ggplot(prison_pop, aes(x = year, y = inmate_per_100k)) +
  geom_line() +
  theme_classic() +
  ylab("Inmates per 100,000 CA residents") +
  geom_vline(data = filter(events, 
                           name %in% c("AB 109", "Prop 47", "Prop 57", "COVID-19")),
             aes(color = effect,
                 xintercept = year))
  
