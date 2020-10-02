library(dplyr)
library(tidyr)
library(pdftools)
library(stringr)
# set working directory to current directory "MissDemeanors/scripts/

# function to read in pdf table from prison population files
read_pdf_table <- function(yr){
  pdf = paste0("../data/CA_prison_pop_reports/July_", yr, ".pdf")
  d <- pdftools::pdf_data(pdf)[2][[1]] # extract table 1 on page 2 of pdf
  # test
  df <- d %>% # try to turn value into a number
    dplyr::mutate(number = as.numeric(stringr::str_replace_all(text, ",", "") %>%
                                        stringr::str_replace_all(., "-", "0") %>%
                                        as.numeric(.))) # get rid of commas and change any single dashes - to zeros
  # which rows have numbers?
  rows_with_numbers <- unique(df$y[!is.na(df$number)])
  # how many columns of numbers does each row have?
  df_numbers <- df %>%
    dplyr::filter(!is.na(number)) %>%
    dplyr::group_by(y) %>%
    left_join(., dplyr::summarise(., n = n()), by = "y")
  # restructure numbers into a dataframe with relevant columns
  # some relevant rows have all 6 columns
  df6 <- df_numbers %>%
    filter(., n == 6) %>%
    dplyr::arrange(x) %>% # sort from lowest to highest y (top to bottom of page)
    dplyr::arrange(y) %>%
    dplyr::mutate(col = rep(c(1:6), length(unique(y)))) %>%
    tidyr::pivot_wider(data = ., id_cols = y,
                       names_from = col, names_prefix = "col",
                       values_from = number)
  # other rows are missing column 2 (no CIVIL ADDICTs)
  # and so only have 5 number columns
  df5 <- df_numbers %>%
    filter(n == 5) %>%
    dplyr::arrange(x) %>% # sort from lowest to highest y (top to bottom of page)
    dplyr::arrange(y) %>%
    dplyr::mutate(col = rep(c(1, 3, 4, 5, 6), length(unique(y)))) %>% # some rows are missing col 2
    tidyr::pivot_wider(data = ., id_cols = y,
                       names_from = col, names_prefix = "col",
                       values_from = number) %>%
    dplyr::mutate(col2 = 0) # if no civil addict listed, it's zero
  # combine all rows back together and add names:
  df_combined <- bind_rows(df6, df5) %>%
    left_join(., arrange(df, x) %>%
                group_by(., y) %>% # for every unique y
                summarise(name = first(text)), # names are always the first x
              by = "y") %>%
    dplyr::arrange(y) %>%
    dplyr::mutate(Year = yr) %>%
    dplyr::select(name, y, Year, starts_with("col")) %>%
    dplyr::ungroup()
  # column names I'm interested in:
  # (all files have columns in the same order)
  # header <- c("FELON/", "CIVIL", "TOTAL", "DESIGN", "PERCENT", "STAFFED")
  #FELON/     CIVIL               DESIGN    PERCENT    STAFFED
  #OTHER     ADDICT     TOTAL    CAPACITY   OCCUPIED   CAPACITY
  colnames(df_combined) <- c("abrev", "Yindex", "Year", "FelOth", "CivAdd", "Total", "DesCap", "PercCap", "StaffCap")
  return(df_combined)
}

# extract data for years 1995 to 2018 (2020 and 2019 have a diff. format)
yrs = 1995:2018
data_1995_2018 = do.call(rbind, lapply(yrs, function(yr) read_pdf_table(yr)))

# extract 2019 and 2020 separately in for loop below because 
# they have a different format in the pdf
PercCap<-NULL
name<-NULL
abrev<-NULL
DesCap<-NULL
FelOth<-NULL
StafCap<-NULL
Total<-NULL
Yindex<-NULL
Year<-NULL
for (l in c(2019,2020)){
  pdf = paste0("../data/CA_prison_pop_reports/July_",l,".pdf")
  d <- pdftools::pdf_data(pdf)[2][[1]]
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

data_2019_2020 <- as.data.frame(Occup19_20) %>%
  dplyr::mutate(Year = as.numeric(Year),
                Yindex = as.numeric(Yindex),
                StafCap = as.numeric(stringr::str_replace_all(StafCap, ",", "")),
                FelOth = as.numeric(stringr::str_replace_all(FelOth, ",", "")),
                PercCap = as.numeric(stringr::str_replace_all(PercCap, ",", "")),
                DesCap = as.numeric(stringr::str_replace_all(DesCap, ",", "")),
                abrev = stringr::str_extract(abrev, "[A-Z]+"))
data_all_yrs <- bind_rows(data_1995_2017, data_2019_2020) 
write.csv(data_all_yrs, "Occupancy.csv")