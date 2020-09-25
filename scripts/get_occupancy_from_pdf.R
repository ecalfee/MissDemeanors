require(dplyr)
#require(tabulizer)
library(pdftools)
# this script extracts data on prison occupancy from pdfs


# extract the table
pdf <- "../data/CA_prison_pop_reports/July_2020_simple.pdf"
out <- pdftools::pdf_text(pdf)
d <- pdftools::pdf_data(pdf)
#out <- tabulizer::extract_tables(pdf)
#webpage <- "https://web.archive.org/web/20071214123130/http://www.cdcr.ca.gov/Visitors/docs/20071015-WEBmapbooklet.pdf"
#out <- tabulizer::extract_tables(webpage)
dates <- pdftools::pdf_text(webpage)
