require(dplyr)
#require(tabulizer)
library(pdftools)
# this script extracts data on prison occupancy from pdfs

# extract the table
file_names <- dir("/Users/darien/GitDarien/MissDemeanors/data/CA_prison_pop_reports")
July<-file_names[grep("July_",file_names)]
July<-July[-26]
for (k in unique(July)){
  Object = paste("../data/CA_prison_pop_reports/",k,sep="")
  assign(k,Object)
}
July89_93<-July[1:5]
July95_05<-July[6:14]
July08<-July[15]
July09_10<-July[16:17]
July11_12<-July[18:19]
July13<-July[20]
July14_18<-July[21:24]
July19_20<-July[25:26]

PDFs_July08<-unlist(lapply(July08,function(s) eval(parse(text=s))))
PDFs_July09_10<-unlist(lapply(July09_10,function(s) eval(parse(text=s))))
PDFs_July11_12<-unlist(lapply(July11_12,function(s) eval(parse(text=s))))
PDFs_July13<-unlist(lapply(July13,function(s) eval(parse(text=s))))
PDFs_July14_18<-unlist(lapply(July14_18,function(s) eval(parse(text=s))))
PDFs_July19_20<-unlist(lapply(July19_20,function(s) eval(parse(text=s))))

####2008
PercCap<-NULL
name<-NULL
abrev<-NULL
DesCap<-NULL
FelOth<-NULL
StafCap<-NULL
Total<-NULL
CivAdd<-NULL
Yindex<-NULL
Year<-NULL
for (l in unique(PDFs_July08)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>121&yvals!=650&yvals!=642&yvals!=625&yvals!=599&yvals!=582&yvals!=497&yvals!=514&yvals!=480]

  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    n<-grep('^[A-Za-z ()]+$', yi$text, value = TRUE)
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-grep('^[1-9]', yi$text, value = TRUE)
    PC<-B[4]
    DC<-B[3]
    FO<-B[1]
    SC<-B[5]
    TO<-B[2]
    CA<-B[6]
    name<-c(name,nm)
    abrev<-c(abrev,abr)
    PercCap<-c(PercCap,PC)
    DesCap<-c(DesCap,DC)
    FelOth<-c(FelOth,FO)
    StafCap<-c(StafCap,SC)
    Total<-c(Total,TO)
    CivAdd<-c(CivAdd,CA)
    Yindex<-c(Yindex,yi$y[1])
    Year<-c(Year,l)
  }
  Occup08<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup08<-as.data.frame(Occup08)
  Occup08$name[Occup08$name=="NA NA"]<-"Total"
  Occup08$name[Occup08$name=="NA FEMALE"]<-"Female Total"
  Occup08$name[Occup08$name=="NA MALE"]<-"Male Total"
}

######2009-2010
PercCap<-NULL
name<-NULL
abrev<-NULL
DesCap<-NULL
FelOth<-NULL
StafCap<-NULL
Total<-NULL
CivAdd<-NULL
Yindex<-NULL
Year<-NULL
for (l in unique(PDFs_July09_10)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>121&yvals!=642&yvals!=633&yvals!=616&yvals!=591&yvals!=574&yvals!=505&yvals!=488&yvals!=471]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    n<-grep('^[A-Za-z ()]+$', yi$text, value = TRUE)
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-grep('^[1-9]', yi$text, value = TRUE)
    PC<-B[4]
    DC<-B[3]
    FO<-B[1]
    SC<-B[5]
    TO<-B[2]
    CA<-B[6]
    name<-c(name,nm)
    abrev<-c(abrev,abr)
    PercCap<-c(PercCap,PC)
    DesCap<-c(DesCap,DC)
    FelOth<-c(FelOth,FO)
    StafCap<-c(StafCap,SC)
    Total<-c(Total,TO)
    CivAdd<-c(CivAdd,CA)
    Yindex<-c(Yindex,yi$y[1])
    Year<-c(Year,l)
  }
  Occup910<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup910<-as.data.frame(Occup910)
  Occup910$name[Occup910$name=="NA NA"]<-"Total"
  Occup910$name[Occup910$name=="NA FEMALE"]<-"Female Total"
  Occup910$name[Occup910$name=="NA MALE"]<-"Male Total"
}

########2011-2012
PercCap<-NULL
name<-NULL
abrev<-NULL
DesCap<-NULL
FelOth<-NULL
StafCap<-NULL
Total<-NULL
CivAdd<-NULL
Yindex<-NULL
Year<-NULL
for (l in unique(PDFs_July11_12)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>121&yvals!=744&yvals!=565&yvals!=514&yvals!=471&yvals!=437]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    n<-grep('^[A-Za-z ()]+$', yi$text, value = TRUE)
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-grep('^[1-9]', yi$text, value = TRUE)
    PC<-B[4]
    DC<-B[3]
    FO<-B[1]
    SC<-B[5]
    TO<-B[2]
    CA<-B[6]
    name<-c(name,nm)
    abrev<-c(abrev,abr)
    PercCap<-c(PercCap,PC)
    DesCap<-c(DesCap,DC)
    FelOth<-c(FelOth,FO)
    StafCap<-c(StafCap,SC)
    Total<-c(Total,TO)
    CivAdd<-c(CivAdd,CA)
    Yindex<-c(Yindex,yi$y[1])
    Year<-c(Year,l)
  }
  Occup11_12<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup11_12<-as.data.frame(Occup11_12)
  Occup11_12$name[Occup11_12$name=="NA NA"]<-"Total"
  Occup11_12$name[Occup11_12$name=="NA FEMALE"]<-"Female Total"
  Occup11_12$name[Occup11_12$name=="NA MALE"]<-"Male Total"
}

########2013
PercCap<-NULL
name<-NULL
abrev<-NULL
DesCap<-NULL
FelOth<-NULL
StafCap<-NULL
Total<-NULL
CivAdd<-NULL
Yindex<-NULL
Year<-NULL
for (l in unique(PDFs_July13)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>121&yvals!=744&yvals!=591&yvals!=539&yvals!=497&yvals!=463]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    n<-grep('^[A-Za-z ()]+$', yi$text, value = TRUE)
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-grep('^[1-9]', yi$text, value = TRUE)
    PC<-B[4]
    DC<-B[3]
    FO<-B[1]
    SC<-B[5]
    TO<-B[2]
    CA<-B[6]
    name<-c(name,nm)
    abrev<-c(abrev,abr)
    PercCap<-c(PercCap,PC)
    DesCap<-c(DesCap,DC)
    FelOth<-c(FelOth,FO)
    StafCap<-c(StafCap,SC)
    Total<-c(Total,TO)
    CivAdd<-c(CivAdd,CA)
    Yindex<-c(Yindex,yi$y[1])
    Year<-c(Year,l)
  }
  Occup13<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup13<-as.data.frame(Occup13)
  Occup13$name[Occup13$name=="NA NA"]<-"Total"
  Occup13$name[Occup13$name=="NA FEMALE"]<-"Female Total"
  Occup13$name[Occup13$name=="NA MALE"]<-"Male Total"
}

########2014-2018
PercCap<-NULL
name<-NULL
abrev<-NULL
DesCap<-NULL
FelOth<-NULL
StafCap<-NULL
Total<-NULL
CivAdd<-NULL
Yindex<-NULL
Year<-NULL
for (l in unique(PDFs_July14_18)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>121&yvals!=744&yvals!=582&yvals!=539&yvals!=497&yvals!=463]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    n<-grep('^[A-Za-z ()]+$', yi$text, value = TRUE)
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-grep('^[1-9]', yi$text, value = TRUE)
    PC<-B[4]
    DC<-B[3]
    FO<-B[1]
    SC<-B[5]
    TO<-B[2]
    CA<-B[6]
    name<-c(name,nm)
    abrev<-c(abrev,abr)
    PercCap<-c(PercCap,PC)
    DesCap<-c(DesCap,DC)
    FelOth<-c(FelOth,FO)
    StafCap<-c(StafCap,SC)
    Total<-c(Total,TO)
    CivAdd<-c(CivAdd,CA)
    Yindex<-c(Yindex,yi$y[1])
    Year<-c(Year,l)
  }
  Occup14_18<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup14_18<-as.data.frame(Occup14_18)
  Occup14_18$name[Occup14_18$name=="NA NA"]<-"Total"
  Occup14_18$name[Occup14_18$name=="NA FEMALE"]<-"Female Total"
  Occup14_18$name[Occup14_18$name=="NA MALE"]<-"Male Total"
}


PDFs_July19_20<-unlist(lapply(July19_20,function(s) eval(parse(text=s))))
for (l in unique(PDFs_July19_20)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>180&yvals!=557&yvals!=738]
  categories<-subset(d1,d1$y==133)
  
  PercCap<-NULL
  name<-NULL
  abrev<-NULL
  DesCap<-NULL
  FelOth<-NULL
  StafCap<-NULL
  Year<-NULL
  Yindex<-NULL
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    n<-grep('^[A-Za-z ()]+$', yi$text, value = TRUE)
    nm<-paste(n[1:(length(n)-1)],collapse=" ")
    abr<-n[length(n)]
    B<-grep('^[1-9]', yi$text, value = TRUE)
    PC<-B[3]
    DC<-B[2]
    FO<-B[1]
    SC<-B[4]
    name<-c(name,nm)
    abrev<-c(abrev,abr)
    PercCap<-c(PercCap,PC)
    DesCap<-c(DesCap,DC)
    FelOth<-c(FelOth,FO)
    StafCap<-c(StafCap,SC)
    Yindex<-c(Yindex,yi$y[1])
    Year<-c(Year,l)
  }
  Occup19_20<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Yindex,Year)
  Occup19_20<-as.data.frame(Occup19_20)
  Occup19_20$name[Occup19_20$name=="NA NA"]<-"Total"
  Occup19_20$name[Occup19_20$name=="NA FEMALE"]<-"Female Total"
  Occup19_20$name[Occup19_20$name=="NA MALE"]<-"Male Total"
  
}

#out <- tabulizer::extract_tables(pdf)
#webpage <- "https://web.archive.org/web/20071214123130/http://www.cdcr.ca.gov/Visitors/docs/20071015-WEBmapbooklet.pdf"
#out <- tabulizer::extract_tables(webpage)
dates <- pdftools::pdf_text(webpage)
