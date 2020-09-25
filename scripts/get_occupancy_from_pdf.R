require(dplyr)
#require(tabulizer)
library(pdftools)
# this script extracts data on prison occupancy from pdfs


# extract the table
pdf <- "../data/CA_prison_pop_reports/July_2020_simple.pdf"
out <- pdftools::pdf_text(pdf)
d <- pdftools::pdf_data(pdf)
d1<-d[[1]]
yvals<-unique(d1$y)
yvals<-yvals[yvals>180&yvals!=557&yvals!=738]
categories<-subset(d1,d1$y==133)
subset(d1,d1$y==184)

PercCap<-NULL
name<-NULL
abrev<-NULL
DesCap<-NULL
FelOth<-NULL
StafCap<-NULL
for (i in unique(yvals)){
  yi<-subset(d1,d1$y==i) 
   nm<-paste(yi$text[yi$space==TRUE], collapse=" ")
   abr<-yi$text[which(yi$space==FALSE)[1]]
   PC<-yi$text[which(yi$space==FALSE)[4]]
   DC<-yi$text[which(yi$space==FALSE)[3]]
   FO<-yi$text[which(yi$space==FALSE)[2]]
   SC<-yi$text[which(yi$space==FALSE)[5]]
   name<-c(name,nm)
   abrev<-c(abrev,abr)
   PercCap<-c(PercCap,PC)
   DesCap<-c(DesCap,DC)
   FelOth<-c(FelOth,FO)
   StafCap<-c(StafCap,SC)
}
Occup<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap)

#out <- tabulizer::extract_tables(pdf)
#webpage <- "https://web.archive.org/web/20071214123130/http://www.cdcr.ca.gov/Visitors/docs/20071015-WEBmapbooklet.pdf"
#out <- tabulizer::extract_tables(webpage)
dates <- pdftools::pdf_text(webpage)
