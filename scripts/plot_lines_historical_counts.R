# this script makes a line plot of historical prison
# population sizes through time
require(dplyr)
require(ggplot2)
require(stringr)
require(here) # paths relative to project directory "MissDemeanors/"

# read in data
# prison population totals
prison <- read.table(here("data/total_inmate_pop_by_year.txt"),
                     header = T, sep = "\t", stringsAsFactors = F)
# take early historical census data from every 10 years 1850-1900
# and estimate intermediate values for each year
# using linear interpolation
pop_old_10yr <- read.table(here("data/CA_census_1850-1900.csv"),
                           header = T, stringsAsFactors = F,
                           sep = ",") %>%
  mutate(pop_count = as.integer(stringr::str_replace_all(pop_count, ",", ""))) # get rid of commas in numbers
pop_old_1yr <- approx(x = pop_old_10yr$year, y = pop_old_10yr$pop_count, 
                                             xout = 1851:1900, method = "linear") %>%
  as.data.frame(.) %>%
  data.table::setnames(c("year", "pop_count"))
pop_recent <- read.table(here("data/CA_total_population_formatted_1900-2019.csv"),
                  header = T, sep = ",", stringsAsFactors = F) %>%
  # convert strings with commas to numbers, and multiply by 1000 because Population is in units thousands
  mutate(pop_count = as.integer(stringr::str_replace_all(Population, ",", ""))*1000) %>%
  dplyr::rename(year = July_year) %>%
  dplyr::select(year, pop_count) %>%
  dplyr::filter(year > 1900) # filter out small overlap b/c earlier source is more precise

# occupancy by prison
prison2 <- readRDS(here("data/full_data.RDS")) %>%
  mutate(Year = as.integer(Year)) %>%
  group_by(Year) %>%
  summarise(inmate_count = sum(Inmates))

# combine data and calculate inmates per 100,000 total population
d <- bind_rows(pop_old_1yr, pop_recent) %>%
  left_join(prison, ., by = "year") %>%
  mutate(inmate_per_100k = inmate_count/pop_count*100000)


# plot
ggplot(d, aes(x = year, y = inmate_count)) +
  geom_line() +
  theme_light() +
  ylab("Total inmates in CA prisons")
ggplot(d, aes(x = year, y = pop_count/10^6)) +
  geom_line() +
  theme_light() +
  ylab("Total CA residents (millions)")
ggplot(d, aes(x = year, y = inmate_per_100k)) +
  geom_line() +
  theme_light() +
  ylab("CA prisoners per 100,000 residents")
