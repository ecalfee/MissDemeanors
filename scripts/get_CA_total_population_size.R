# this script combines data sources for total CA population
# from the Census and Dept. of Finance Estimates
# to get yearly population size estimates 1851-2020

require(dplyr)
require(stringr)
require(readxl)
require(here) # paths relative to project directory "MissDemeanors/"


# take early historical census data from every 10 years 1850-1900
# source: https://www.census.gov/history/pdf/california_res_pop-jan2018.pdf
census_1850_1900 <- read.table(here("data/CA_census_1850-1900.csv"),
                               header = T, stringsAsFactors = F,
                               sep = ",") %>%
  mutate(pop_estimate = as.integer(stringr::str_replace_all(pop_count, ",", ""))) # get rid of commas in numbers
# and estimate intermediate values for each year 1851-1900
# using linear interpolation
pop_1851_1900 <- approx(x = census_1850_1900$year, y = census_1850_1900$pop_estimate, 
                        xout = 1851:1900, method = "linear") %>%
  as.data.frame(.) %>%
  data.table::setnames(c("year", "pop_estimate")) %>%
  dplyr::mutate(pop_estimate = round(pop_estimate)) # round to nearest whole person

# get Dept. of Finance CA population size estimates 1901-2019
# downloaded from: http://www.dof.ca.gov/Forecasting/Demographics/Estimates/E-7/documents/E-7_Report_1900-July_2019w.xlsx
pop_1901_2019 <- readxl::read_xlsx(here("data/E-7_Report_1900-July_2019w.xlsx"),
                                   sheet = 2, skip = 4)[ , 1:2] %>%
  data.table::setnames(c("year", "population")) %>%
  # convert strings with commas to numbers, and multiply by 1000 because population is in units thousands
  dplyr::mutate(pop_estimate = as.integer(stringr::str_replace_all(population, ",", ""))*1000) %>%
  dplyr::filter(year != 1900) %>% # use census data instead for 1900 because it's more accurate
  dplyr::select(year, pop_estimate)

# get Dept. of Finance CA population size estimate 2020
# dowloaded from: http://www.dof.ca.gov/Forecasting/Demographics/Estimates/e-1/documents/E-1_2020_InternetVersion.xlsx
pop_2020 <- readxl::read_xlsx(here("data/E-1_2020_InternetVersion.xlsx"),
                              sheet = 2, skip = 3) %>%
  .[.$"State/County/City" == "California", "Total Population 1/1/2020"] %>% # first row is California
  dplyr::rename(pop_estimate = "Total Population 1/1/2020") %>%
  dplyr::mutate(year = 2020)
# combine data
pop_all_yrs <- bind_rows(pop_1851_1900, pop_1901_2019, pop_2020)
  
# write output file
write.table(pop_all_yrs, here("data/total_CA_pop_by_year.txt"),
            col.names = T, row.names = F, sep = "\t", quote = F)
