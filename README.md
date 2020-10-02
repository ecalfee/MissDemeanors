# Team MissDemeanors
# California Proposition 20: Criminal Sentencing, Parole, and DNA Collection Initiative (2020)
## Questions: 
- What is the historical context for Prop. 20? Does California currently have relatively fewer/shorter sentences or more/longer senetences?
- How much room does the California prison system have for an increase in inmates?

## View the Rshiny App!
https://ecalfee.shinyapps.io/missdemeanors/

## Data Sources
# Prison Addresses
- California Department of Corrections and Rehabilitation website https://www.cdcr.ca.gov/facility-locator/adult-institutions

california-population-1850-2019.csv
# Total California Population Data
- California Population 1900-2019 compiled from US Census Bureau Population Estimates https://www.macrotrends.net/states/california/population
<a href='https://www.macrotrends.net/states/california/population'>California Population 1900-2019</a>
- California Population 1850, 1860, 1870, 1880 and 1900 from US Census
https://www.census.gov/history/pdf/california_res_pop-jan2018.pdf
(saved as data/CA_census_1850-1900.csv)

# Total California Prison Population Data
The first state prison opened in 1851 and 3 historical reports contain summaries covering all years 1851-2000, after which the more modern yearly reports of inmate totals by facility
These public reports were obtained by emailing the Office of Research Data Concierge Service: data.requests@cdcr.ca.gov 
- data/calprisd/calprisd1851_1945.pdf
- data/calprisd/CalPrisd2000.pdf
- data/calprisd/calprisd1960.pdf, from which we made a screenshot image of the relevant table:
data/calprisd/calprisd1960_Tbl1_pg18_total_prison_pop.png


- Individual Prison Occupancy Data
The California Dept. of Corrections and Rehabilitation maintains monthly prison population reports for 1989-present
These public reports were obtained by emailing the Office of Research Data Concierge Service: data.requests@cdcr.ca.gov 
We obtained reports for the month of July 1989-2020. We plot this data starting in 1995 because earlier reports used different facility naming conventions.
- data/CA_prison_pop_reports

## TO FIX
- NEED TO ADD WHERE WE GOT ANY 'FILLED IN' DATA, e.g. missing prison addresses or design capacity, how were these obtained? (add url)
-San Quentin State Prison (SQ) and California City Correctional Facility (CAC) do have addresses but do not have any occupancy data
- Rio Cosumnes Correctional Center (RIO), Santa Rita County Jail (SRTA), and Pitchess Detention Center (PDC) are all county jails that are within this database.


## Analysis steps and scripts


