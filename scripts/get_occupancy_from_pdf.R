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
July95<-July[6]
July96<-July[7]
July97<-July[8]
July98<-July[9]
July99<-July[10]
July01_04<-July[11:13]
July05<-July[14]
July08<-July[15]
July09_10<-July[16:17]
July11_12<-July[18:19]
July13<-July[20]
July14_18<-July[21:24]
July19_20<-July[25:26]

PDFs_July95<-unlist(lapply(July95,function(s) eval(parse(text=s))))
PDFs_July96<-unlist(lapply(July96,function(s) eval(parse(text=s))))
PDFs_July97<-unlist(lapply(July97,function(s) eval(parse(text=s))))
PDFs_July98<-unlist(lapply(July98,function(s) eval(parse(text=s))))
PDFs_July99<-unlist(lapply(July99,function(s) eval(parse(text=s))))
PDFs_July01_04<-unlist(lapply(July01_04,function(s) eval(parse(text=s))))
PDFs_July05<-unlist(lapply(July05,function(s) eval(parse(text=s))))
PDFs_July08<-unlist(lapply(July08,function(s) eval(parse(text=s))))
PDFs_July09_10<-unlist(lapply(July09_10,function(s) eval(parse(text=s))))
PDFs_July11_12<-unlist(lapply(July11_12,function(s) eval(parse(text=s))))
PDFs_July13<-unlist(lapply(July13,function(s) eval(parse(text=s))))
PDFs_July14_18<-unlist(lapply(July14_18,function(s) eval(parse(text=s))))
PDFs_July19_20<-unlist(lapply(July19_20,function(s) eval(parse(text=s))))

####1995
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
for (l in unique(PDFs_July95)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>103&yvals!=428&yvals!=446&yvals!=464&yvals!=546&yvals!=564&yvals!=582&yvals!=600&yvals!=672]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i)
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-n[-which(n==notname)]
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-yi$text[(length(yi$text)-5):length(yi$text)]
    PC<-B[5]
    DC<-B[4]
    FO<-B[1]
    SC<-B[6]
    TO<-B[3]
    CA<-B[2]
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
  Occup95<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup95<-as.data.frame(Occup95)
  Occup95$name[Occup95$name=="NA NA"]<-"Total"
  Occup95$name[Occup95$name=="NA FEMALE"]<-"Female Total"
  Occup95$name[Occup95$name=="NA MALE"]<-"Male Total"
}

####1996
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
for (l in unique(PDFs_July96)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>103&yvals!=455&yvals!=473&yvals!=491&yvals!=573&yvals!=591&yvals!=609&yvals!=627&yvals!=654]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-yi$text[(length(yi$text)-5):length(yi$text)]
    PC<-B[5]
    DC<-B[4]
    FO<-B[1]
    SC<-B[6]
    TO<-B[3]
    CA<-B[2]
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
  Occup96<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup96<-as.data.frame(Occup96)
  Occup96$name[Occup96$name=="NA NA"]<-"Total"
  Occup96$name[Occup96$name=="NA FEMALE"]<-"Female Total"
  Occup96$name[Occup96$name=="NA MALE"]<-"Male Total"
}

####1997
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
for (l in unique(PDFs_July97)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>103&yvals!=464&yvals!=482&yvals!=500&yvals!=582&yvals!=600&yvals!=618&yvals!=636&yvals!=654]
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[-A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-yi$text[(length(yi$text)-5):length(yi$text)]
    PC<-B[5]
    DC<-B[4]
    FO<-B[1]
    SC<-B[6]
    TO<-B[3]
    CA<-B[2]
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
  Occup97<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup97<-as.data.frame(Occup97)
  Occup97$name[Occup97$name=="NA NA"]<-"Total"
  Occup97$name[Occup97$name=="NA FEMALE"]<-"Female Total"
  Occup97$name[Occup97$name=="NA MALE"]<-"Male Total"
}

####1998
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
for (l in unique(PDFs_July98)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d1$x[d1$text=="FELON/"]
  FelonXRange<-c(FelonX-20,FelonX+20)
  CivilAddictX<-d1$x[d1$text=="CIVIL"]
  CivilAddictXRange<-c(CivilAddictX-20,CivilAddictX+20)
  TotalX<-d1$x[d1$text=="TOTAL"]
  TotalXRange<-c(TotalX-20,TotalX+20)
  DesignCapX<-d1$x[d1$text=="DESIGN"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d1$x[d1$text=="PERCENT"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d1$x[d1$text=="STAFFED"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>103&yvals!=491&yvals!=473&yvals!=509&yvals!=627&yvals!=591&yvals!=609&yvals!=645&yvals!=663]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z ()]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-subset(yi,yi$x>200) #This subsets off the name of the prison, which all occur at x indexes smaller than 218
    PC<-B$text[B$x>=PercentOccXRange[1]&B$x<=PercentOccXRange[2]] #here 411 is the x index for all Percent Occupied Values
    DC<-B$text[B$x>=DesignCapXRange[1]&B$x<=DesignCapXRange[2]]
    FO<-B$text[B$x>=FelonXRange[1]&B$x<=FelonXRange[2]]
    SC<-B$text[B$x>=StaffedX]
    TO<-B$text[B$x>=TotalXRange[1]&B$x<=TotalXRange[2]]
    #Okay the civil addict column was a little tricky
    #This line says, if there exists a number within the x range of the civil addict column than store that number in CA
    #otherwise store 0 in CA
    CA<-ifelse(any(B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]),B$text[B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]],0) #if there is a civil addict # then input that, else put zero
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
  Occup98<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup98<-as.data.frame(Occup98)
  Occup98$name[Occup98$name=="NA NA"]<-"Total"
  Occup98$name[Occup98$name=="NA FEMALE"]<-"Female Total"
  Occup98$name[Occup98$name=="NA MALE"]<-"Male Total"
}

####1999
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
for (l in unique(PDFs_July99)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d1$x[d1$text=="FELON/"]
  FelonXRange<-c(FelonX-20,FelonX+20)
  CivilAddictX<-d1$x[d1$text=="CIVIL"]
  CivilAddictXRange<-c(CivilAddictX-20,CivilAddictX+20)
  TotalX<-d1$x[d1$text=="TOTAL"]
  TotalXRange<-c(TotalX-20,TotalX+20)
  DesignCapX<-d1$x[d1$text=="DESIGN"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d1$x[d1$text=="PERCENT"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d1$x[d1$text=="STAFFED"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>103&yvals!=491&yvals!=473&yvals!=509&yvals!=627&yvals!=591&yvals!=609&yvals!=645&yvals!=663]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-subset(yi,yi$x>190) #This subsets off the name of the prison, which all occur at x indexes smaller than 218
    PC<-B$text[B$x>=PercentOccXRange[1]&B$x<=PercentOccXRange[2]] #here 411 is the x index for all Percent Occupied Values
    DC<-B$text[B$x>=DesignCapXRange[1]&B$x<=DesignCapXRange[2]]
    FO<-B$text[B$x>=FelonXRange[1]&B$x<=FelonXRange[2]]
    SC<-B$text[B$x>=StaffedX]
    TO<-B$text[B$x>=TotalXRange[1]&B$x<=TotalXRange[2]]
    #Okay the civil addict column was a little tricky
    #This line says, if there exists a number within the x range of the civil addict column than store that number in CA
    #otherwise store 0 in CA
    CA<-ifelse(any(B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]),B$text[B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]],0) #if there is a civil addict # then input that, else put zero
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
  Occup99<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup99<-as.data.frame(Occup99)
  Occup99$name[Occup99$name=="NA NA"]<-"Total"
  Occup99$name[Occup99$name=="NA FEMALE"]<-"Female Total"
  Occup99$name[Occup99$name=="NA MALE"]<-"Male Total"
}



######2001-2004
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
for (l in unique(PDFs_July01_04)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d1$x[d1$text=="FELON/"]
  FelonXRange<-c(FelonX-20,FelonX+30)
  CivilAddictX<-d1$x[d1$text=="CIVIL"]
  CivilAddictXRange<-c(CivilAddictX-20,CivilAddictX+20)
  TotalX<-d1$x[d1$text=="TOTAL"]
  TotalXRange<-c(TotalX-20,TotalX+20)
  DesignCapX<-d1$x[d1$text=="DESIGN"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d1$x[d1$text=="PERCENT"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d1$x[d1$text=="STAFFED"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>103&yvals!=481&yvals!=500&yvals!=518&yvals!=599&yvals!=617&yvals!=636&yvals!=654&yvals!=663]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-subset(yi,yi$x>190) #This subsets off the name of the prison, which all occur at x indexes smaller than 218
    PC<-B$text[B$x>=PercentOccXRange[1]&B$x<=PercentOccXRange[2]] #here 411 is the x index for all Percent Occupied Values
    DC<-B$text[B$x>=DesignCapXRange[1]&B$x<=DesignCapXRange[2]]
    FO<-B$text[B$x>=FelonXRange[1]&B$x<=FelonXRange[2]]
    SC<-B$text[B$x>=StaffedX]
    TO<-B$text[B$x>=TotalXRange[1]&B$x<=TotalXRange[2]]
    #Okay the civil addict column was a little tricky
    #This line says, if there exists a number within the x range of the civil addict column than store that number in CA
    #otherwise store 0 in CA
    CA<-ifelse(any(B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]),B$text[B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]],0) #if there is a civil addict # then input that, else put zero
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
  Occup01_04<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup01_04<-as.data.frame(Occup01_04)
  Occup01_04$name[Occup01_04$name=="NA NA"]<-"Total"
  Occup01_04$name[Occup01_04$name=="NA FEMALE"]<-"Female Total"
  Occup01_04$name[Occup01_04$name=="NA MALE"]<-"Male Total"
}


####2005
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
for (l in unique(PDFs_July05)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d1$x[d1$text=="FELON/"]
  FelonXRange<-c(FelonX-20,FelonX+20)
  CivilAddictX<-d1$x[d1$text=="CIVIL"]
  CivilAddictXRange<-c(CivilAddictX-20,CivilAddictX+20)
  TotalX<-d1$x[d1$text=="TOTAL"]
  TotalXRange<-c(TotalX-20,TotalX+20)
  DesignCapX<-d1$x[d1$text=="DESIGN"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d1$x[d1$text=="PERCENT"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d1$x[d1$text=="STAFFED"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>103&yvals!=500&yvals!=518&yvals!=536&yvals!=617&yvals!=636&yvals!=654&yvals!=672&yvals!=681]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-subset(yi,yi$x>200) #This subsets off the name of the prison, which all occur at x indexes smaller than 218
    PC<-B$text[B$x>=PercentOccXRange[1]&B$x<=PercentOccXRange[2]] #here 411 is the x index for all Percent Occupied Values
    DC<-B$text[B$x>=DesignCapXRange[1]&B$x<=DesignCapXRange[2]]
    FO<-B$text[B$x>=FelonXRange[1]&B$x<=FelonXRange[2]]
    SC<-B$text[B$x>=StaffedX]
    TO<-B$text[B$x>=TotalXRange[1]&B$x<=TotalXRange[2]]
    #Okay the civil addict column was a little tricky
    #This line says, if there exists a number within the x range of the civil addict column than store that number in CA
    #otherwise store 0 in CA
    CA<-ifelse(any(B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]),B$text[B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]],0) #if there is a civil addict # then input that, else put zero
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
  Occup05<-cbind(name,abrev,PercCap,DesCap,FelOth,StafCap,Total,CivAdd,Yindex,Year)
  Occup05<-as.data.frame(Occup05)
  Occup05$name[Occup05$name=="NA NA"]<-"Total"
  Occup05$name[Occup05$name=="NA FEMALE"]<-"Female Total"
  Occup05$name[Occup05$name=="NA MALE"]<-"Male Total"
}


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
  
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d1$x[d1$text=="FELON/"]
  FelonXRange<-c(FelonX-20,FelonX+30)
  CivilAddictX<-d1$x[d1$text=="CIVIL"]
  CivilAddictXRange<-c(CivilAddictX-20,CivilAddictX+20)
  TotalX<-d1$x[d1$text=="TOTAL"]
  TotalXRange<-c(TotalX-20,TotalX+20)
  DesignCapX<-d1$x[d1$text=="DESIGN"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d1$x[d1$text=="PERCENT"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d1$x[d1$text=="STAFFED"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>121&yvals!=650&yvals!=642&yvals!=625&yvals!=599&yvals!=582&yvals!=497&yvals!=514&yvals!=480]

  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-subset(yi,yi$x>200) #This subsets off the name of the prison, which all occur at x indexes smaller than 218
    PC<-B$text[B$x>=PercentOccXRange[1]&B$x<=PercentOccXRange[2]] #here 411 is the x index for all Percent Occupied Values
    DC<-B$text[B$x>=DesignCapXRange[1]&B$x<=DesignCapXRange[2]]
    FO<-B$text[B$x>=FelonXRange[1]&B$x<=FelonXRange[2]]
    SC<-B$text[B$x>=StaffedX]
    TO<-B$text[B$x>=TotalXRange[1]&B$x<=TotalXRange[2]]
    #Okay the civil addict column was a little tricky
    #This line says, if there exists a number within the x range of the civil addict column than store that number in CA
    #otherwise store 0 in CA
    CA<-ifelse(any(B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]),B$text[B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]],0) #if there is a civil addict # then input that, else put zero
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
  
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d1$x[d1$text=="FELON/"]
  FelonXRange<-c(FelonX-20,FelonX+30)
  CivilAddictX<-d1$x[d1$text=="CIVIL"]
  CivilAddictXRange<-c(CivilAddictX-20,CivilAddictX+20)
  TotalX<-d1$x[d1$text=="TOTAL"]
  TotalXRange<-c(TotalX-20,TotalX+20)
  DesignCapX<-d1$x[d1$text=="DESIGN"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d1$x[d1$text=="PERCENT"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d1$x[d1$text=="STAFFED"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>121&yvals!=642&yvals!=633&yvals!=616&yvals!=591&yvals!=574&yvals!=505&yvals!=488&yvals!=471]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-subset(yi,yi$x>200) #This subsets off the name of the prison, which all occur at x indexes smaller than 218
    PC<-B$text[B$x>=PercentOccXRange[1]&B$x<=PercentOccXRange[2]] #here 411 is the x index for all Percent Occupied Values
    DC<-B$text[B$x>=DesignCapXRange[1]&B$x<=DesignCapXRange[2]]
    FO<-B$text[B$x>=FelonXRange[1]&B$x<=FelonXRange[2]]
    SC<-B$text[B$x>=StaffedX]
    TO<-B$text[B$x>=TotalXRange[1]&B$x<=TotalXRange[2]]
    #Okay the civil addict column was a little tricky
    #This line says, if there exists a number within the x range of the civil addict column than store that number in CA
    #otherwise store 0 in CA
    CA<-ifelse(any(B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]),B$text[B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]],0) #if there is a civil addict # then input that, else put zero
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
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d1$x[d1$text=="FELON/"]
  FelonXRange<-c(FelonX-20,FelonX+30)
  CivilAddictX<-d1$x[d1$text=="CIVIL"]
  CivilAddictXRange<-c(CivilAddictX-20,CivilAddictX+20)
  TotalX<-d1$x[d1$text=="TOTAL"]
  TotalXRange<-c(TotalX-20,TotalX+20)
  DesignCapX<-d1$x[d1$text=="DESIGN"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d1$x[d1$text=="PERCENT"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d1$x[d1$text=="STAFFED"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>121&yvals!=744&yvals!=565&yvals!=514&yvals!=471&yvals!=437]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-subset(yi,yi$x>200) #This subsets off the name of the prison, which all occur at x indexes smaller than 218
    PC<-B$text[B$x>=PercentOccXRange[1]&B$x<=PercentOccXRange[2]] #here 411 is the x index for all Percent Occupied Values
    DC<-B$text[B$x>=DesignCapXRange[1]&B$x<=DesignCapXRange[2]]
    FO<-B$text[B$x>=FelonXRange[1]&B$x<=FelonXRange[2]]
    SC<-B$text[B$x>=StaffedX]
    TO<-B$text[B$x>=TotalXRange[1]&B$x<=TotalXRange[2]]
    #Okay the civil addict column was a little tricky
    #This line says, if there exists a number within the x range of the civil addict column than store that number in CA
    #otherwise store 0 in CA
    CA<-ifelse(any(B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]),B$text[B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]],0) #if there is a civil addict # then input that, else put zero
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
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d1$x[d1$text=="FELON/"]
  FelonXRange<-c(FelonX-20,FelonX+30)
  CivilAddictX<-d1$x[d1$text=="CIVIL"]
  CivilAddictXRange<-c(CivilAddictX-20,CivilAddictX+20)
  TotalX<-d1$x[d1$text=="TOTAL"]
  TotalXRange<-c(TotalX-20,TotalX+20)
  DesignCapX<-d1$x[d1$text=="DESIGN"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d1$x[d1$text=="PERCENT"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d1$x[d1$text=="STAFFED"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>121&yvals!=744&yvals!=591&yvals!=539&yvals!=497&yvals!=463]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-subset(yi,yi$x>200) #This subsets off the name of the prison, which all occur at x indexes smaller than 218
    PC<-B$text[B$x>=PercentOccXRange[1]&B$x<=PercentOccXRange[2]] #here 411 is the x index for all Percent Occupied Values
    DC<-B$text[B$x>=DesignCapXRange[1]&B$x<=DesignCapXRange[2]]
    FO<-B$text[B$x>=FelonXRange[1]&B$x<=FelonXRange[2]]
    SC<-B$text[B$x>=StaffedX]
    TO<-B$text[B$x>=TotalXRange[1]&B$x<=TotalXRange[2]]
    #Okay the civil addict column was a little tricky
    #This line says, if there exists a number within the x range of the civil addict column than store that number in CA
    #otherwise store 0 in CA
    CA<-ifelse(any(B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]),B$text[B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]],0) #if there is a civil addict # then input that, else put zero
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
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d1$x[d1$text=="FELON/"]
  FelonXRange<-c(FelonX-20,FelonX+30)
  CivilAddictX<-d1$x[d1$text=="CIVIL"]
  CivilAddictXRange<-c(CivilAddictX-20,CivilAddictX+20)
  TotalX<-d1$x[d1$text=="TOTAL"]
  TotalXRange<-c(TotalX-20,TotalX+20)
  DesignCapX<-d1$x[d1$text=="DESIGN"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d1$x[d1$text=="PERCENT"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d1$x[d1$text=="STAFFED"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>121&yvals!=744&yvals!=582&yvals!=539&yvals!=497&yvals!=463]
  
  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[2:length(n)],collapse=" ")
    abr<-n[1]
    B<-subset(yi,yi$x>200) #This subsets off the name of the prison, which all occur at x indexes smaller than 218
    PC<-B$text[B$x>=PercentOccXRange[1]&B$x<=PercentOccXRange[2]] #here 411 is the x index for all Percent Occupied Values
    DC<-B$text[B$x>=DesignCapXRange[1]&B$x<=DesignCapXRange[2]]
    FO<-B$text[B$x>=FelonXRange[1]&B$x<=FelonXRange[2]]
    SC<-B$text[B$x>=StaffedX]
    TO<-B$text[B$x>=TotalXRange[1]&B$x<=TotalXRange[2]]
    #Okay the civil addict column was a little tricky
    #This line says, if there exists a number within the x range of the civil addict column than store that number in CA
    #otherwise store 0 in CA
    CA<-ifelse(any(B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]),B$text[B$x>=CivilAddictXRange[1]&B$x<=CivilAddictXRange[2]],0) #if there is a civil addict # then input that, else put zero
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
PercCap<-NULL
name<-NULL
abrev<-NULL
DesCap<-NULL
FelOth<-NULL
StafCap<-NULL
Total<-NULL
Yindex<-NULL
Year<-NULL

for (l in unique(PDFs_July19_20)){
  out <- pdftools::pdf_text(l)
  d <- pdftools::pdf_data(l)[2]
  d1<-d[[1]]
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d1$x[d1$text=="Felon/"]
  FelonXRange<-c(FelonX-20,FelonX+30)
  DesignCapX<-d1$x[d1$text=="Design"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d1$x[d1$text=="Percent"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d1$x[d1$text=="Staffed"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  yvals<-unique(d1$y)
  yvals<-yvals[yvals>180&yvals!=557&yvals!=738]

  for (i in unique(yvals)){
    yi<-subset(d1,d1$y==i) 
    yi<-yi[order(yi$x),]
    n<-grep('^[A-Za-z () -]+$', yi$text, value = TRUE)
    notname<-n[grep('^-$',n)]
    n<-if(length(notname)>=1) n[-which(n==notname)] else n
    nm<-paste(n[1:(length(n)-1)],collapse=" ")
    abr<-n[length(n)]
    B<-subset(yi,yi$x>200) #This subsets off the name of the prison, which all occur at x indexes smaller than 218
    PC<-B$text[B$x>=PercentOccXRange[1]&B$x<=PercentOccXRange[2]] #here 411 is the x index for all Percent Occupied Values
    DC<-B$text[B$x>=DesignCapXRange[1]&B$x<=DesignCapXRange[2]]
    FO<-B$text[B$x>=FelonXRange[1]&B$x<=FelonXRange[2]]
    SC<-B$text[B$x>=StaffedX]
   
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


Occupancy<-rbind(Occup95,Occup96,Occup97,Occup98,Occup99,Occup01_04,Occup05,Occup08,Occup910,Occup11_12,Occup13,Occup14_18)
detach("package:dplyr", unload = TRUE)
library(plyr)
Occupancy<-rbind.fill(Occupancy,Occup19_20)
write.csv(Occupancy,"Occupany.csv")

