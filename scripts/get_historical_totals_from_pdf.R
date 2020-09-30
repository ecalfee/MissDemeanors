require(dplyr)
require(pdftools) # newer pdfs, extract text
require(stringr)
require(tesseract) # older pdfs, analyze image for text
require(magick) # pre-process png
# this script extracts historical data on CA total prison population
# from 'calprisd' pdfs obtained from Data Concierge Service-Office of Research
# Division of Correctional Policy Research and Internal Oversight
# California Department of Corrections and Rehabilitation
# (916) 255-0185, Data.Requests@cdcr.ca.gov

# The first state prison opened in 1851 and 2 historical reports
# contain summaries covering most years between then and the 
# more modern yearly reports of inmate totals by facility
report1851 <- "../data/calprisd/calprisd1851_1945.pdf"
report2000 <- "../data/calprisd/CalPrisd2000.pdf"
png1960 <- "../data/calprisd/calprisd1960_Tbl1_pg18_total_prison_pop.png"

# from the 2000 report we want Table 6 on page 30
# which gives total institutional population from 1960-2000
table2000 <- pdftools::pdf_data(report2000)[[30]]
# pdf_data extracts every word into a 'text' column
# associated with an x and y position on the page
# find x-values (columns) for years and total inmate population size
x_years <- table2000$x[table2000$text == 1961] # x value for years (e.g. the year 1961 will be found in this column)
x_inmate_counts <- table2000$x[table2000$y == table2000$y[table2000$text == 1961]][3] # third column for any year is the inmate total population #
# extract those 2 columns into a new dataframe
data2000 <- data.frame(year = table2000$text[table2000$x == x_years],
                       inmate_count = table2000$text[table2000$x > x_inmate_counts - 5 & table2000$x <= x_inmate_counts], # larger total populations have 1 more digit and a slightly lower x-position, so I give it a narrow range to find this column
                       stringsAsFactors = F) %>%
  dplyr::mutate(year = as.integer(year), # make character string into a number
                inmate_count = as.integer(stringr::str_replace(inmate_count, ",", ""))) # and remove any commas in the numbers first

# from the 1851-1945 report we want the tables spanning pages 1-4
# we run optical character recognition (OCR)
# to identify text by classifying characters from their image
# in this older 1851-1945 report because the text isn't readily extracted
text1851 <- tesseract::ocr(image = report1851, 
                    # constraining possible symbols to numbers and minus sign greatly improves accuracy
                    engine = tesseract(options = list(tessedit_char_whitelist = "0123456789-")))
# text1851 is a vector with a string of text for each page
# cat(text1851[1]) # e.g. page 1

# get all the lines from the first 4 pages
lines1851 = unlist(stringr::str_split(text1851[1:4], pattern = "\\n")) # split on newline

# from lines with at least 8 elements, take the first and eight column
# which is the year and total inmate population count
data1851 <- do.call(bind_rows, lapply(lines1851, function(x){
  l = stringr::str_split(x, pattern = " ")[[1]] # split on space
  if (length(l) < 8){ 
    return(NULL) # short or blank line
  } else{ # full line
    return(c(year = as.integer(l[1]), 
             inmate_count = as.integer(l[8]))) # return columns 1 and 8
  }
})) 

# From the 1960 report, we save an image of the population totals
# from page 18, Table 1, 1930-1960:
# these numbers are hard to read, so try both small and big versions
# and keep the one that detects the most digits
# to visually check against the original image
image1960 <- magick::image_read(png1960) %>%
  image_scale(., "150") # resizing helps digit recognition
text1960 <- tesseract::ocr(image = image1960, 
                           engine = tesseract(options = list(tessedit_char_whitelist = "0123456789")))

data1960 <- data.frame(year = 1930:1960,
           inmate_count = as.integer(stringr::str_split(text1960, 
                                                 pattern = "\\n")[[1]][1:31])) %>%
  dplyr::filter(year %in% 1946:1959) # some discrepancies between pdfs from CA Dept. of Corrections; default to numbers from larger table
data1960$inmate_count[data1960$year == 1951] <- 11939 # manually fix one number not read correctly by image OCD


# write combined output file
rbind(data1851, data1960, data2000) %>%
  write.table(., file = "../data/total_inmate_pop_by_year.txt", sep = "\t",
              col.names = T, row.names = F, quote = F)
