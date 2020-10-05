# Team MissDemeanors
## Erin Calfee, Darien Satterfield, & Katherine Corn
# California Proposition 20: Criminal Sentencing, Parole, and DNA Collection Initiative (2020)
## Questions:
- What is the historical context for Prop. 20? Does California currently have relatively fewer/shorter sentences or more/longer senetences?
- How much room does the California prison system have for an increase in inmates?

## View the Rshiny App!
https://ecalfee.shinyapps.io/missdemeanors/    

## Data Sources
### Prison Addresses
- California Department of Corrections and Rehabilitation website https://www.cdcr.ca.gov/facility-locator/adult-institutions

### Total California Population Data
- 2020 California Population Estimate Dept. of Finance
http://www.dof.ca.gov/Forecasting/Demographics/Estimates/e-1/documents/E-1_2020_InternetVersion.xlsx
- 1900-2019 California Populaiton Estimates Dept. of Finance
http://www.dof.ca.gov/Forecasting/Demographics/Estimates/E-7/documents/E-7_Report_1900-July_2019w.xlsx
- California Population 1850, 1860, 1870, 1880 and 1900 from US Census
https://www.census.gov/history/pdf/california_res_pop-jan2018.pdf
(saved as data/CA_census_1850-1900.csv)

### Total California Prison Population Data
The first state prison opened in 1851 and 3 historical reports contain summaries covering all years 1851-2000, after which the more modern yearly reports of inmate totals by facility
These public reports were obtained by emailing the Office of Research Data Concierge Service: data.requests@cdcr.ca.gov
- data/calprisd/calprisd1851_1945.pdf
- data/calprisd/CalPrisd2000.pdf
- data/calprisd/calprisd1960.pdf, from which we made a screenshot image of the relevant table:
data/calprisd/calprisd1960_Tbl1_pg18_total_prison_pop.png


### Individual Prison Occupancy Data
The California Dept. of Corrections and Rehabilitation maintains monthly prison population reports for 1989-present
These public reports were obtained by emailing the Office of Research Data Concierge Service: data.requests@cdcr.ca.gov
We obtained reports for the month of July 1989-2020. We extract and plot this data starting in 1995 because earlier reports used different facility naming conventions.
We additionally use 1990-2020 reports for total inmate population numbers for recent years.
- original PDFs saved in data/CA_prison_pop_reports/

### List of major historical legislation affecting prison populations in California
Dowloaded from California Department of Corrections and Rehabilitation Fig 1.2 in 'Offender Data Points Dec. 2018'
https://www.cdcr.ca.gov/research/wp-content/uploads/sites/174/2020/01/201812_DataPoints.pdf
- original pdf saved as data/Data_Points-Dec-2018.pdf

## Filled in data: Data we had to pull individually

### Missing prison addresses (all accessed 27 September 2020)
 - Northern California Women's Facility: https://rs.locationshub.com/location_detail.aspx?id=050-6834&page=13&user=50&cat=cpxuaotmb
- Pitchess Detention Facility: https://locator.lacounty.gov/lac/Location/3036062/los-angeles-county-sheriff---pitchess-detention-center-north-facility
- Rio Cosumnes Correctional Facility: https://www.sacsheriff.com/Pages/Organization/RCCC/RCCC.aspx
- San Bruno County Jail: https://lightinprison.org/us/ca/institutions/san-francisco-county-jail-5/
- No address for Female Rehabilitative Community Correctional Center (FRCC) in Bakersfield so random Bakersfield zip code chosen


### Manual geocoding
A few facilities produced errors latitudes & longitudes when geocoding with tidygeocoder (prisons: California Correctional Institution, California Correctional Center, High Desert State Prison, California Men’s Colony, Mule Creek State Prison)
https://developers.google.com/maps/documentation/geocoding/overview

Three county jail facilities were missing design capacity data. One, Santa Rita Jail, had total capacity in one year, but not in others. We copied capacity for that year into all years for that institution. The other two institutions (Pitchess Detention Center and Rio Cosumnes Correctional Center), we filled the "design capacity" column with "NA". While all inmates are included in the total population, we don't plot any of the jail facilities on the map because we do not have data on the other jail inmates, and therefore % capacity.


## Analysis steps and scripts

### Prison Population Map
1. Pull prison occupancy data from .pdfs, clean into a table, and save to .csv file (1995-2020) | Script: get_occupancy_from_pdf.R
2. Clean prison occupancy csv and save to .RDS; includes labeling by percent occupancy bin, male/female, prison/county jail | Script: occupancy_cleaner.R
3. Download prison location data by scraping CA Corrections website and then geocoding addresses. Some missing addresses must be manually entered, and some erroneous locations need to be manually fixed. Save to .RDS | Script: lat_long_scrape.R
4. Merge geocodes with prison occupancy data | Script: merge_data.R
5. Plot a map by year of prison occupancy per prison | Script: plot_CA_prisonpops.R

### Total Prison Population Per 100,000
1. Read total California population data from .csv and .xlsx files, integrate into a single text file. For years before 1900 we only have data every 10 years from the census (vs. annual population estimates from Dept. of Finance), so we impute annual estimates for these dates using linear approximation | Script: get_CA_total_population_size.R
2. Read total CA prison population data from .pdf and .png files | Script: get_historical_totals_from_pdf.R
3. Plot lines plot of ratios of CA prison populations per total population, linked with major CA events | Script: plot_lines_historical_counts.R

### Rshiny
1. With each of the plots, combine plots into a single large plot and integrate into a shiny slider | Script: app.R


## Software requirements
To run this code, open MissDemeanors as an R project and run the scripts in RStudio. We used R version 4.0.2 (2020-06-22) with the following packages:
 maps_3.3.0
 ggplot2_3.3.2
 readxl_1.3.1
 tidygeocoder_1.0.1
 rvest_0.3.5       
 xml2_1.3.2         
 readr_1.3.1
 here_0.1           
 stringr_1.4.0      
 pdftools_2.3.1    
 tidyr_1.1.0        
 dplyr_1.0.2     
 cowplot_1.0.0
 here_0.1
 magick_2.4.0
 tesseract_4.1
