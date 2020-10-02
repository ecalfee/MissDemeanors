

PercCap<-NULL
name<-NULL
abrev<-NULL
DesCap<-NULL
FelOth<-NULL
StafCap<-NULL
Total<-NULL
Yindex<-NULL
Year<-NULL

yr<-c(2019,2020)
for (l in unique(yr)){
  pdf = paste0("../data/CA_prison_pop_reports/July_",l,".pdf")
  d <- pdftools::pdf_data(pdf)[2][[1]]
  #################################
  #these identify the x values that span the column of each variable
  #the first letter of the column header is usually roughly 20 units into the column
  #and the column will span 50 units past that first letter
  FelonX<-d$x[d$text=="Felon/"]
  FelonXRange<-c(FelonX-20,FelonX+30)
  DesignCapX<-d$x[d$text=="Design"]
  DesignCapXRange<-c(DesignCapX-20,DesignCapX+20)
  PercentOccX<-d$x[d$text=="Percent"]
  PercentOccXRange<-c(PercentOccX-20,PercentOccX+20)
  StaffedX<-d$x[d$text=="Staffed"]
  #StaffedXRange<-c(PercentOccX-20,PercentOccX+20)
  yvals<-unique(d$y)
  yvals<-yvals[yvals>180&yvals!=557&yvals!=738]
  
  for (i in unique(yvals)){
    yi<-subset(d,d$y==i) 
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
  Occup19_20$abrev[Occup19_20$name=="Institution"]<-"INSTITUTIONS/CAMPS"
  Occup19_20$abrev[Occup19_20$name=="Female"]<-"FEMALE"
  Occup19_20$name[Occup19_20$name=="Female"]<-"Female Total"
  Occup19_20$abrev[Occup19_20$name=="Male"]<-"MALE"
  Occup19_20$name[Occup19_20$name=="Male"]<-"Male Total"
  
}

Occup19_20<-as.data.frame(Occup19_20)

