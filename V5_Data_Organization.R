#Perimeter Analysis V5: Data Organization (Sequential Extraction) 7/13/2023
#Matt Harris 

#Load Packages: 
library(MuMIn)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(Hmisc)

#Set workspace: 
setwd("C:/Users/matth/Desktop/Method_1_Analysis/Perimeter_Analysis_V4/Sequential_MP/Originals")

#Load Data: 
D_points_60<-read.csv("FE_MP60.csv")
D_points_120<-read.csv("FE_MP120.csv")
D_points_240<-read.csv("FE_MP240.csv")

#Make list of columns to join.
LF_2001_List<-Cs(LF_AZ_2001, LF_CO_2001, LF_NM_2001, LF_UT_2001)
LF_2012_List<-Cs(LF_AZ_2012, LF_CO_2012, LF_NM_2012, LF_UT_2012)
LF_2016_List<-Cs(LF_AZ_2016, LF_CO_2016, LF_NM_2016, LF_UT_2016)
LF_2020_List<-Cs(LF_AZ_2020, LF_CO_2020, LF_NM_2020, LF_UT_2020)
LF_2001_List<-unlist(LF_2001_List)
LF_2012_List<-unlist(LF_2012_List)
LF_2016_List<-unlist(LF_2016_List)
LF_2020_List<-unlist(LF_2020_List)

#Merge LF data: 
D_points_60 = unite(D_points_60, LF_2001, LF_2001_List, remove = F, na.rm=T)
D_points_60 = unite(D_points_60, LF_2012, LF_2012_List, remove = F, na.rm=T)
D_points_60 = unite(D_points_60, LF_2016, LF_2016_List, remove = F, na.rm=T)
D_points_60 = unite(D_points_60, LF_2020, LF_2020_List, remove = F, na.rm=T)

#Cut out duplicates: 
D_points_60$LF_2001<-str_sub_all(D_points_60$LF_2001,0, 4)
D_points_60$LF_2012<-str_sub_all(D_points_60$LF_2012,0, 4)
D_points_60$LF_2016<-str_sub_all(D_points_60$LF_2016,0, 4)
D_points_60$LF_2020<-str_sub_all(D_points_60$LF_2020,0, 4)

#Merge SWReGAP data: 
SW_List<-Cs(SWReGAP_az_landcover, SWReGAP_co_landcover, SWReGAP_nm_landcover, SWReGAP_ut_landcover)
SW_List<-unlist(SW_List)

#Merge: 
D_points_60 = unite(D_points_60, SWReGAP, SW_List, remove = F, na.rm=T)

#Cut out duplicates: 
D_points_60$SWReGAP<-str_sub_all(D_points_60$SWReGAP,0, 3)

#Repeat for 120
#Merge LF data: 
D_points_120 = unite(D_points_120, LF_2001, LF_2001_List, remove = F, na.rm=T)
D_points_120 = unite(D_points_120, LF_2012, LF_2012_List, remove = F, na.rm=T)
D_points_120 = unite(D_points_120, LF_2016, LF_2016_List, remove = F, na.rm=T)
D_points_120 = unite(D_points_120, LF_2020, LF_2020_List, remove = F, na.rm=T)

#Cut out duplicates: 
D_points_120$LF_2001<-str_sub_all(D_points_120$LF_2001,0, 4)
D_points_120$LF_2012<-str_sub_all(D_points_120$LF_2012,0, 4)
D_points_120$LF_2016<-str_sub_all(D_points_120$LF_2016,0, 4)
D_points_120$LF_2020<-str_sub_all(D_points_120$LF_2020,0, 4)

#Merge: 
D_points_120 = unite(D_points_120, SWReGAP, SW_List, remove = F, na.rm=T)

#Cut out duplicates: 
D_points_120$SWReGAP<-str_sub_all(D_points_120$SWReGAP,0, 3)


#Repeat for 240
#Merge LF data: 
D_points_240 = unite(D_points_240, LF_2001, LF_2001_List, remove = F, na.rm=T)
D_points_240 = unite(D_points_240, LF_2012, LF_2012_List, remove = F, na.rm=T)
D_points_240 = unite(D_points_240, LF_2016, LF_2016_List, remove = F, na.rm=T)
D_points_240 = unite(D_points_240, LF_2020, LF_2020_List, remove = F, na.rm=T)

#Cut out duplicates: 
D_points_240$LF_2001<-str_sub_all(D_points_240$LF_2001,0, 4)
D_points_240$LF_2012<-str_sub_all(D_points_240$LF_2012,0, 4)
D_points_240$LF_2016<-str_sub_all(D_points_240$LF_2016,0, 4)
D_points_240$LF_2020<-str_sub_all(D_points_240$LF_2020,0, 4)

#Merge: 
D_points_240 = unite(D_points_240, SWReGAP, SW_List, remove = F, na.rm=T)

#Cut out duplicates: 
D_points_240$SWReGAP<-str_sub_all(D_points_240$SWReGAP,0, 3)


#Begin organizing Fire year, FSC, Prefire_NDVI, and LF into DSF

#Get Year:
D_points_60$FireYear<-str_sub_all(D_points_60$Event_ID,14,-5)

D_points_120$FireYear<-str_sub_all(D_points_120$Event_ID,14,-5)

D_points_240$FireYear<-str_sub_all(D_points_240$Event_ID,14,-5)

#Relate years to unburned counterpart:
#60
D_points_60$FireYear<-as.numeric(D_points_60$FireYear)

D_points_60 <-D_points_60 %>%
  fill(FireYear, .direction = "up")  %>%
  as.data.frame()

#120
D_points_120$FireYear<-as.numeric(D_points_120$FireYear)

D_points_120 <-D_points_120 %>%
  fill(FireYear, .direction = "up")  %>%
  as.data.frame()

#240 
D_points_240$FireYear<-as.numeric(D_points_240$FireYear)

D_points_240 <-D_points_240 %>%
  fill(FireYear, .direction = "up")  %>%
  as.data.frame()

#Create data source files for all T data sets: (Prior Burn will be addressed individually)
D_points_60$vpd_ds<- NA
D_points_60$def_ds<- NA
D_points_60$Tmax_ds<- NA
D_points_60$PFNDVI_ds<- NA
D_points_60$LFYear_ds <- NA

D_points_120$vpd_ds<- NA
D_points_120$def_ds<- NA
D_points_120$Tmax_ds<- NA
D_points_120$PFNDVI_ds<- NA
D_points_120$LFYear_ds <- NA

D_points_240$vpd_ds<- NA
D_points_240$def_ds<- NA
D_points_240$Tmax_ds<- NA
D_points_240$PFNDVI_ds<- NA
D_points_240$LFYear_ds<- NA

#Rename temporal data sets adding in fire year: 
###60
#vpd
D_points_60 <- D_points_60 %>%
  mutate(vpd_ds = paste0("vpd_", FireYear))

#def
D_points_60 <- D_points_60 %>%
  mutate(def_ds = paste0("TerraClimate_def_", FireYear))

#Tmax
D_points_60 <- D_points_60 %>%
  mutate(Tmax_ds = paste0("tmax_", FireYear))

#Prefire NDVI 
D_points_60 <- D_points_60 %>%
  mutate(PFNDVI_ds = paste0("Prefire_", FireYear))

#Landfire years 
D_points_60 <- D_points_60 %>%
  mutate(LFYear_ds = paste0("LF_", FireYear))

###120
#vpd
D_points_120 <- D_points_120 %>%
  mutate(vpd_ds = paste0("vpd_", FireYear))

#def
D_points_120 <- D_points_120 %>%
  mutate(def_ds = paste0("TerraClimate_def_", FireYear))

#Tmax
D_points_120 <- D_points_120 %>%
  mutate(Tmax_ds = paste0("tmax_", FireYear))

#Prefire NDVI 
D_points_120 <- D_points_120 %>%
  mutate(PFNDVI_ds = paste0("Prefire_", FireYear))

#Landfire years 
D_points_120 <- D_points_120 %>%
  mutate(LFYear_ds = paste0("LF_", FireYear))

###240
#vpd
D_points_240 <- D_points_240 %>%
  mutate(vpd_ds = paste0("vpd_", FireYear))

#def
D_points_240 <- D_points_240 %>%
  mutate(def_ds = paste0("TerraClimate_def_", FireYear))

#Tmax
D_points_240 <- D_points_240 %>%
  mutate(Tmax_ds = paste0("tmax_", FireYear))

#Prefire NDVI 
D_points_240 <- D_points_240 %>%
  mutate(PFNDVI_ds = paste0("Prefire_", FireYear))

#Landfire years 
D_points_240 <- D_points_240 %>%
  mutate(LFYear_ds = paste0("LF_", FireYear))

#############Prep ranges: 

dfPF<- data.frame(year=seq(1985,2020,1), range= c(rep("1985_1989", 10),rep("1990_1994", 5), rep("1995_1999", 5), rep("2000_2004",5), rep("2005_2009", 5), rep("2010_2014", 6)))
#Match Prefire NDVI to the above ranges. 

#Fill Prefire NDVI
#60
D_points_60$PFNDVI_dsf=NA  
for (i in 1:nrow(D_points_60)){
  print(i)
  range= dfPF$range[which(dfPF$year==D_points_60$FireYear[i])]
  colnam=paste0("Prefire_", range)
  D_points_60$PFNDVI_dsf[i]=D_points_60[i,colnam]
}

#120
D_points_120$PFNDVI_dsf=NA  
for (i in 1:nrow(D_points_120)){
  print(i)
  range= dfPF$range[which(dfPF$year==D_points_120$FireYear[i])]
  colnam=paste0("Prefire_", range)
  D_points_120$PFNDVI_dsf[i]=D_points_120[i,colnam]
}

#240
D_points_240$PFNDVI_dsf=NA  
for (i in 1:nrow(D_points_240)){
  print(i)
  range= dfPF$range[which(dfPF$year==D_points_240$FireYear[i])]
  colnam=paste0("Prefire_", range)
  D_points_240$PFNDVI_dsf[i]=D_points_240[i,colnam]
}

#Prep range data frame for LANDFIRE data: 
dfLF<- data.frame(year=seq(1985,2020,1), range= c(rep("_2001", 27),rep("_2012", 4), rep("_2016", 5)))

#Fill LF DSFs with values based on above data frame. 
#60
D_points_60$LFYear_dsf=NA  
for (i in 1:nrow(D_points_60)){
  print(i)
  range= dfLF$range[which(dfLF$year==D_points_60$FireYear[i])]
  colnam=paste0("LF", range)
  D_points_60$LFYear_dsf[i]=D_points_60[i,colnam]
}

#120
D_points_120$LFYear_dsf=NA  
for (i in 1:nrow(D_points_120)){
  print(i)
  range= dfLF$range[which(dfLF$year==D_points_120$FireYear[i])]
  colnam=paste0("LF", range)
  D_points_120$LFYear_dsf[i]=D_points_120[i,colnam]
}

#240
D_points_240$LFYear_dsf=NA  
for (i in 1:nrow(D_points_240)){
  print(i)
  range= dfLF$range[which(dfLF$year==D_points_240$FireYear[i])]
  colnam=paste0("LF", range)
  D_points_240$LFYear_dsf[i]=D_points_240[i,colnam]
}

#Fill FSC source column with values based on fire year designated in the prior step:
#60 
#vpd
D_points_60$vpd_dsf=NA  
for (i in 1:nrow(D_points_60)){
  print(i)
  colnam=D_points_60$vpd_ds[i]
  D_points_60$vpd_dsf[i]=D_points_60[i,colnam]
}
#def
D_points_60$def_dsf=NA  
for (i in 1:nrow(D_points_60)){
  print(i)
  colnam=D_points_60$def_ds[i]
  D_points_60$def_dsf[i]=D_points_60[i,colnam]
}
#Tmax
D_points_60$Tmax_dsf=NA  
for (i in 1:nrow(D_points_60)){
  print(i)
  colnam=D_points_60$Tmax_ds[i]
  D_points_60$Tmax_dsf[i]=D_points_60[i,colnam]
}

#120
#vpd
D_points_120$vpd_dsf=NA  
for (i in 1:nrow(D_points_120)){
  print(i)
  colnam=D_points_120$vpd_ds[i]
  D_points_120$vpd_dsf[i]=D_points_120[i,colnam]
}
#def
D_points_120$def_dsf=NA  
for (i in 1:nrow(D_points_120)){
  print(i)
  colnam=D_points_120$def_ds[i]
  D_points_120$def_dsf[i]=D_points_120[i,colnam]
}
#Tmax
D_points_120$Tmax_dsf=NA  
for (i in 1:nrow(D_points_120)){
  print(i)
  colnam=D_points_120$Tmax_ds[i]
  D_points_120$Tmax_dsf[i]=D_points_120[i,colnam]
}

#240
#vpd
D_points_240$vpd_dsf=NA  
for (i in 1:nrow(D_points_240)){
  print(i)
  colnam=D_points_240$vpd_ds[i]
  D_points_240$vpd_dsf[i]=D_points_240[i,colnam]
}
#def
D_points_240$def_dsf=NA  
for (i in 1:nrow(D_points_240)){
  print(i)
  colnam=D_points_240$def_ds[i]
  D_points_240$def_dsf[i]=D_points_240[i,colnam]
}
#Tmax
D_points_240$Tmax_dsf=NA  
for (i in 1:nrow(D_points_240)){
  print(i)
  colnam=D_points_240$Tmax_ds[i]
  D_points_240$Tmax_dsf[i]=D_points_240[i,colnam]
}

#Now its time to address prior burn. Unlike V3 I now have a raster for every year
#Step 1 Fill all NON NA Rows with the year value from column name.
#Step 2 Make a DSF for Prior_Burn: Seperate by commas to make a string. 
#Step 3 Make a year since fire column by subtracting FireYear by Prior_Burn: Ta Da!

####################################Prior burn is still not finished: Continue working this section. 

##Replace every listed value with its respective column name: couldn't get my loop to work so the following is some ugly hard coding.
#60
D_points_60$PB_1984<-replace(D_points_60$PB_1984, D_points_60$PB_1984>0, 1984)
D_points_60$PB_1985<-replace(D_points_60$PB_1985, D_points_60$PB_1985>0, 1985)
D_points_60$PB_1986<-replace(D_points_60$PB_1986, D_points_60$PB_1986>0, 1986)
D_points_60$PB_1987<-replace(D_points_60$PB_1987, D_points_60$PB_1987>0, 1987)
D_points_60$PB_1988<-replace(D_points_60$PB_1988, D_points_60$PB_1988>0, 1988)
D_points_60$PB_1989<-replace(D_points_60$PB_1989, D_points_60$PB_1989>0, 1989)
D_points_60$PB_1990<-replace(D_points_60$PB_1990, D_points_60$PB_1990>0, 1990)
D_points_60$PB_1991<-replace(D_points_60$PB_1991, D_points_60$PB_1991>0, 1991)
D_points_60$PB_1992<-replace(D_points_60$PB_1992, D_points_60$PB_1992>0, 1992)
D_points_60$PB_1993<-replace(D_points_60$PB_1993, D_points_60$PB_1993>0, 1993)
D_points_60$PB_1994<-replace(D_points_60$PB_1994, D_points_60$PB_1994>0, 1994)
D_points_60$PB_1995<-replace(D_points_60$PB_1995, D_points_60$PB_1995>0, 1995)
D_points_60$PB_1996<-replace(D_points_60$PB_1996, D_points_60$PB_1996>0, 1996)
D_points_60$PB_1997<-replace(D_points_60$PB_1997, D_points_60$PB_1997>0, 1997)
D_points_60$PB_1998<-replace(D_points_60$PB_1998, D_points_60$PB_1998>0, 1998)
D_points_60$PB_1999<-replace(D_points_60$PB_1999, D_points_60$PB_1999>0, 1999)
D_points_60$PB_2000<-replace(D_points_60$PB_2000, D_points_60$PB_2000>0, 2000)
D_points_60$PB_2001<-replace(D_points_60$PB_2001, D_points_60$PB_2001>0, 2001)
D_points_60$PB_2002<-replace(D_points_60$PB_2002, D_points_60$PB_2002>0, 2002)
D_points_60$PB_2003<-replace(D_points_60$PB_2003, D_points_60$PB_2003>0, 2003)
D_points_60$PB_2004<-replace(D_points_60$PB_2004, D_points_60$PB_2004>0, 2004)
D_points_60$PB_2005<-replace(D_points_60$PB_2005, D_points_60$PB_2005>0, 2005)
D_points_60$PB_2006<-replace(D_points_60$PB_2006, D_points_60$PB_2006>0, 2006)
D_points_60$PB_2007<-replace(D_points_60$PB_2007, D_points_60$PB_2007>0, 2007)
D_points_60$PB_2008<-replace(D_points_60$PB_2008, D_points_60$PB_2008>0, 2008)
D_points_60$PB_2009<-replace(D_points_60$PB_2009, D_points_60$PB_2009>0, 2009)
D_points_60$PB_2010<-replace(D_points_60$PB_2010, D_points_60$PB_2010>0, 2010)
D_points_60$PB_2011<-replace(D_points_60$PB_2011, D_points_60$PB_2011>0, 2011)
D_points_60$PB_2012<-replace(D_points_60$PB_2012, D_points_60$PB_2012>0, 2012)
D_points_60$PB_2013<-replace(D_points_60$PB_2013, D_points_60$PB_2013>0, 2013)
D_points_60$PB_2014<-replace(D_points_60$PB_2014, D_points_60$PB_2014>0, 2014)
D_points_60$PB_2015<-replace(D_points_60$PB_2015, D_points_60$PB_2015>0, 2015)
D_points_60$PB_2016<-replace(D_points_60$PB_2016, D_points_60$PB_2016>0, 2016)
D_points_60$PB_2017<-replace(D_points_60$PB_2017, D_points_60$PB_2017>0, 2017)
D_points_60$PB_2018<-replace(D_points_60$PB_2018, D_points_60$PB_2018>0, 2018)
D_points_60$PB_2019<-replace(D_points_60$PB_2019, D_points_60$PB_2019>0, 2019)
D_points_60$PB_2020<-replace(D_points_60$PB_2020, D_points_60$PB_2020>0, 2020)

#120
D_points_120$PB_1984<-replace(D_points_120$PB_1984, D_points_120$PB_1984>0, 1984)
D_points_120$PB_1985<-replace(D_points_120$PB_1985, D_points_120$PB_1985>0, 1985)
D_points_120$PB_1986<-replace(D_points_120$PB_1986, D_points_120$PB_1986>0, 1986)
D_points_120$PB_1987<-replace(D_points_120$PB_1987, D_points_120$PB_1987>0, 1987)
D_points_120$PB_1988<-replace(D_points_120$PB_1988, D_points_120$PB_1988>0, 1988)
D_points_120$PB_1989<-replace(D_points_120$PB_1989, D_points_120$PB_1989>0, 1989)
D_points_120$PB_1990<-replace(D_points_120$PB_1990, D_points_120$PB_1990>0, 1990)
D_points_120$PB_1991<-replace(D_points_120$PB_1991, D_points_120$PB_1991>0, 1991)
D_points_120$PB_1992<-replace(D_points_120$PB_1992, D_points_120$PB_1992>0, 1992)
D_points_120$PB_1993<-replace(D_points_120$PB_1993, D_points_120$PB_1993>0, 1993)
D_points_120$PB_1994<-replace(D_points_120$PB_1994, D_points_120$PB_1994>0, 1994)
D_points_120$PB_1995<-replace(D_points_120$PB_1995, D_points_120$PB_1995>0, 1995)
D_points_120$PB_1996<-replace(D_points_120$PB_1996, D_points_120$PB_1996>0, 1996)
D_points_120$PB_1997<-replace(D_points_120$PB_1997, D_points_120$PB_1997>0, 1997)
D_points_120$PB_1998<-replace(D_points_120$PB_1998, D_points_120$PB_1998>0, 1998)
D_points_120$PB_1999<-replace(D_points_120$PB_1999, D_points_120$PB_1999>0, 1999)
D_points_120$PB_2000<-replace(D_points_120$PB_2000, D_points_120$PB_2000>0, 2000)
D_points_120$PB_2001<-replace(D_points_120$PB_2001, D_points_120$PB_2001>0, 2001)
D_points_120$PB_2002<-replace(D_points_120$PB_2002, D_points_120$PB_2002>0, 2002)
D_points_120$PB_2003<-replace(D_points_120$PB_2003, D_points_120$PB_2003>0, 2003)
D_points_120$PB_2004<-replace(D_points_120$PB_2004, D_points_120$PB_2004>0, 2004)
D_points_120$PB_2005<-replace(D_points_120$PB_2005, D_points_120$PB_2005>0, 2005)
D_points_120$PB_2006<-replace(D_points_120$PB_2006, D_points_120$PB_2006>0, 2006)
D_points_120$PB_2007<-replace(D_points_120$PB_2007, D_points_120$PB_2007>0, 2007)
D_points_120$PB_2008<-replace(D_points_120$PB_2008, D_points_120$PB_2008>0, 2008)
D_points_120$PB_2009<-replace(D_points_120$PB_2009, D_points_120$PB_2009>0, 2009)
D_points_120$PB_2010<-replace(D_points_120$PB_2010, D_points_120$PB_2010>0, 2010)
D_points_120$PB_2011<-replace(D_points_120$PB_2011, D_points_120$PB_2011>0, 2011)
D_points_120$PB_2012<-replace(D_points_120$PB_2012, D_points_120$PB_2012>0, 2012)
D_points_120$PB_2013<-replace(D_points_120$PB_2013, D_points_120$PB_2013>0, 2013)
D_points_120$PB_2014<-replace(D_points_120$PB_2014, D_points_120$PB_2014>0, 2014)
D_points_120$PB_2015<-replace(D_points_120$PB_2015, D_points_120$PB_2015>0, 2015)
D_points_120$PB_2016<-replace(D_points_120$PB_2016, D_points_120$PB_2016>0, 2016)
D_points_120$PB_2017<-replace(D_points_120$PB_2017, D_points_120$PB_2017>0, 2017)
D_points_120$PB_2018<-replace(D_points_120$PB_2018, D_points_120$PB_2018>0, 2018)
D_points_120$PB_2019<-replace(D_points_120$PB_2019, D_points_120$PB_2019>0, 2019)
D_points_120$PB_2020<-replace(D_points_120$PB_2020, D_points_120$PB_2020>0, 2020)

#240
D_points_240$PB_1984<-replace(D_points_240$PB_1984, D_points_240$PB_1984>0, 1984)
D_points_240$PB_1985<-replace(D_points_240$PB_1985, D_points_240$PB_1985>0, 1985)
D_points_240$PB_1986<-replace(D_points_240$PB_1986, D_points_240$PB_1986>0, 1986)
D_points_240$PB_1987<-replace(D_points_240$PB_1987, D_points_240$PB_1987>0, 1987)
D_points_240$PB_1988<-replace(D_points_240$PB_1988, D_points_240$PB_1988>0, 1988)
D_points_240$PB_1989<-replace(D_points_240$PB_1989, D_points_240$PB_1989>0, 1989)
D_points_240$PB_1990<-replace(D_points_240$PB_1990, D_points_240$PB_1990>0, 1990)
D_points_240$PB_1991<-replace(D_points_240$PB_1991, D_points_240$PB_1991>0, 1991)
D_points_240$PB_1992<-replace(D_points_240$PB_1992, D_points_240$PB_1992>0, 1992)
D_points_240$PB_1993<-replace(D_points_240$PB_1993, D_points_240$PB_1993>0, 1993)
D_points_240$PB_1994<-replace(D_points_240$PB_1994, D_points_240$PB_1994>0, 1994)
D_points_240$PB_1995<-replace(D_points_240$PB_1995, D_points_240$PB_1995>0, 1995)
D_points_240$PB_1996<-replace(D_points_240$PB_1996, D_points_240$PB_1996>0, 1996)
D_points_240$PB_1997<-replace(D_points_240$PB_1997, D_points_240$PB_1997>0, 1997)
D_points_240$PB_1998<-replace(D_points_240$PB_1998, D_points_240$PB_1998>0, 1998)
D_points_240$PB_1999<-replace(D_points_240$PB_1999, D_points_240$PB_1999>0, 1999)
D_points_240$PB_2000<-replace(D_points_240$PB_2000, D_points_240$PB_2000>0, 2000)
D_points_240$PB_2001<-replace(D_points_240$PB_2001, D_points_240$PB_2001>0, 2001)
D_points_240$PB_2002<-replace(D_points_240$PB_2002, D_points_240$PB_2002>0, 2002)
D_points_240$PB_2003<-replace(D_points_240$PB_2003, D_points_240$PB_2003>0, 2003)
D_points_240$PB_2004<-replace(D_points_240$PB_2004, D_points_240$PB_2004>0, 2004)
D_points_240$PB_2005<-replace(D_points_240$PB_2005, D_points_240$PB_2005>0, 2005)
D_points_240$PB_2006<-replace(D_points_240$PB_2006, D_points_240$PB_2006>0, 2006)
D_points_240$PB_2007<-replace(D_points_240$PB_2007, D_points_240$PB_2007>0, 2007)
D_points_240$PB_2008<-replace(D_points_240$PB_2008, D_points_240$PB_2008>0, 2008)
D_points_240$PB_2009<-replace(D_points_240$PB_2009, D_points_240$PB_2009>0, 2009)
D_points_240$PB_2010<-replace(D_points_240$PB_2010, D_points_240$PB_2010>0, 2010)
D_points_240$PB_2011<-replace(D_points_240$PB_2011, D_points_240$PB_2011>0, 2011)
D_points_240$PB_2012<-replace(D_points_240$PB_2012, D_points_240$PB_2012>0, 2012)
D_points_240$PB_2013<-replace(D_points_240$PB_2013, D_points_240$PB_2013>0, 2013)
D_points_240$PB_2014<-replace(D_points_240$PB_2014, D_points_240$PB_2014>0, 2014)
D_points_240$PB_2015<-replace(D_points_240$PB_2015, D_points_240$PB_2015>0, 2015)
D_points_240$PB_2016<-replace(D_points_240$PB_2016, D_points_240$PB_2016>0, 2016)
D_points_240$PB_2017<-replace(D_points_240$PB_2017, D_points_240$PB_2017>0, 2017)
D_points_240$PB_2018<-replace(D_points_240$PB_2018, D_points_240$PB_2018>0, 2018)
D_points_240$PB_2019<-replace(D_points_240$PB_2019, D_points_240$PB_2019>0, 2019)
D_points_240$PB_2020<-replace(D_points_240$PB_2020, D_points_240$PB_2020>0, 2020)

PB_List<-Cs(PB_1984, PB_1985, PB_1986, PB_1987, PB_1988, PB_1989, PB_1990, PB_1991, PB_1992, PB_1993, PB_1994, PB_1995, PB_1996, PB_1997, PB_1998, PB_1999, PB_2000, PB_2001, PB_2002, PB_2003, PB_2004, PB_2005, PB_2006, PB_2007, PB_2008, PB_2009, PB_2010, PB_2011, PB_2012, PB_2013, PB_2014, PB_2015, PB_2016, PB_2017, PB_2018, PB_2019, PB_2020)


D_points_60$All_Burns<-NA
D_points_60 = unite(D_points_60, All_Burns, PB_List, sep= ", " ,remove = F, na.rm=T)


D_points_120$All_Burns<-NA
D_points_120 = unite(D_points_120, All_Burns, PB_List, sep= ", " ,remove = F, na.rm=T)


D_points_240$All_Burns<-NA
D_points_240 = unite(D_points_240, All_Burns, PB_List, sep= ", " ,remove = F, na.rm=T)

###Below is addressed with the Most Recent Prior Burn.R script. 
#I have a string of all prior burns now I need to make a column of burns less than the fire year only, then select the most recent of those years. I have tried a variety of selection process for this and non have worked so I will be reaching out to Jared for help. Once I have the most recent prior burn preceding the fire year then I can create my year since fire column.
###############################################################################

#Make CWD Z Score: 
#60
#Make subset between 1985-2015: 
ZR60<-D_points_60[D_points_60$FireYear >= "1986" & D_points_60$FireYear <= "2015", ]
#Make mean for the period and standard dev
ZR60$PeriodMean= mean(ZR60$def_dsf)
ZR60$PeriodSTDEV= sd(ZR60$def_dsf)

D_points_60$PeriodMean= mean(ZR60$def_dsf)
D_points_60$PeriodSTDEV= sd(ZR60$def_dsf)

D_points_60$def_ZScore= (D_points_60$def_dsf - D_points_60$PeriodMean)/D_points_60$PeriodSTDEV

#120
#Make subset between 1985-2015: 
ZR120<-D_points_120[D_points_120$FireYear >= "1986" & D_points_120$FireYear <= "2015", ]
#Make mean for the period and standard dev
ZR120$PeriodMean= mean(ZR120$def_dsf)
ZR120$PeriodSTDEV= sd(ZR120$def_dsf)

D_points_120$PeriodMean= mean(ZR120$def_dsf)
D_points_120$PeriodSTDEV= sd(ZR120$def_dsf)

D_points_120$def_ZScore= (D_points_120$def_dsf - D_points_120$PeriodMean)/D_points_120$PeriodSTDEV

#240
#Make subset between 1985-2015: 
ZR240<-D_points_240[D_points_240$FireYear >= "1986" & D_points_240$FireYear <= "2015", ]
#Make mean for the period and standard dev
ZR240$PeriodMean= mean(ZR240$def_dsf)
ZR240$PeriodSTDEV= sd(ZR240$def_dsf)

D_points_240$PeriodMean= mean(ZR240$def_dsf)
D_points_240$PeriodSTDEV= sd(ZR240$def_dsf)

D_points_240$def_ZScore= (D_points_240$def_dsf - D_points_240$PeriodMean)/D_points_240$PeriodSTDEV



library(data.table)
#Export C2 files: 
#60
fwrite(D_points_60, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Sequential_MP\\C1_Points\\C1_Points_60m.csv", append= FALSE)
#120
fwrite(D_points_120, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Sequential_MP\\C1_Points\\C1_Points_120m.csv", append= FALSE)
#240
fwrite(D_points_240, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Sequential_MP\\C1_Points\\C1_Points_240m.csv", append= FALSE)


################################################################
#Classify cleaned landcover data sets from codes to descriptions: 
#Start with SWReGAP: 

#Set workspace: 
setwd("C:/Users/matth/Desktop/Method_1_Analysis/Perimeter_Analysis_V4/Sequential_MP/Cleaned_Points")

C60M<-read.csv("C60M.csv")
C120M<-read.csv("C120M.csv")
C240M<-read.csv("C240M.csv")
#Load Desc: 
setwd("C:/Users/matth/Desktop/Method_1_Data")

SW_Desc<- read.csv("SWReGAP_Desc.csv")

#Clean SWCodes 
#60
C60M$SWReGAP_Code<- gsub('0_', '',C60M$SWReGAP_Code )
C60M$SWReGAP_Code<- gsub('_0', '',C60M$SWReGAP_Code )
C60M$SWReGAP_Code<- gsub('_', '',C60M$SWReGAP_Code )
#120
C120M$SWReGAP_Code<- gsub('0_', '',C120M$SWReGAP_Code )
C120M$SWReGAP_Code<- gsub('_0', '',C120M$SWReGAP_Code )
C120M$SWReGAP_Code<- gsub('_', '',C120M$SWReGAP_Code )
#240
C240M$SWReGAP_Code<- gsub('0_', '',C240M$SWReGAP_Code )
C240M$SWReGAP_Code<- gsub('_0', '',C240M$SWReGAP_Code )
C240M$SWReGAP_Code<- gsub('_', '',C240M$SWReGAP_Code )

C60M$SWReGAP_Class<-NA
C120M$SWReGAP_Class<-NA
C240M$SWReGAP_Class<-NA

#Matched values in order to fill with LC description. 
C60M$SWReGAP_Class <- SW_Desc$DESCRIPTION[match(C60M$SWReGAP_Code, SW_Desc$Value)]

C120M$SWReGAP_Class <- SW_Desc$DESCRIPTION[match(C120M$SWReGAP_Code, SW_Desc$Value)]

C240M$SWReGAP_Class <- SW_Desc$DESCRIPTION[match(C240M$SWReGAP_Code, SW_Desc$Value)]
#Worked for now repeat for 120 and 240, as well as LF. 

#LF: 
setwd("C:/Users/matth/Desktop/Method_1_Data")
LF_Desc<- read.csv("LF_Desc.csv")

C60M$LANDFIRE_Class<-NA
C120M$LANDFIRE_Class<-NA
C240M$LANDFIRE_Class<-NA

C60M$LANDFIRE_Class <-  LF_Desc$SAF_SRM[match(C60M$LFYear_dsf_Code, LF_Desc$VALUE)]

C120M$LANDFIRE_Class <- LF_Desc$SAF_SRM[match(C120M$LFYear_dsf_Code, LF_Desc$VALUE)]

C240M$LANDFIRE_Class <- LF_Desc$SAF_SRM[match(C240M$LFYear_dsf_Code, LF_Desc$VALUE)]
#All LC descriptions filled! 

#Create aridity metric: Aridity is calculated as the average of the z-scores of Deficit, Tmax and VPD.
C60M$Aridity<- NA
C120M$Aridity<- NA
C240M$Aridity<- NA

#Calc Aridity: 
C60M$Aridity<- (C60M$vpd_dsf + C60M$def_dsf + C60M$Tmax_dsf)/3
C120M$Aridity<- (C120M$vpd_dsf + C120M$def_dsf + C120M$Tmax_dsf)/3
C240M$Aridity<- (C240M$vpd_dsf + C240M$def_dsf + C240M$Tmax_dsf)/3


#######Fix blanks in landcover classes: 

#SW
C60M$SWReGAP_Class<-ifelse(C60M$SWReGAP_Class=="", NA, C60M$SWReGAP_Class)
C120M$SWReGAP_Class<-ifelse(C120M$SWReGAP_Class=="", NA, C120M$SWReGAP_Class)
C240M$SWReGAP_Class<-ifelse(C240M$SWReGAP_Class=="", NA, C240M$SWReGAP_Class)

#LF
C60M$LANDFIRE_Class<-ifelse(C60M$LANDFIRE_Class=="", NA, C60M$LANDFIRE_Class)
C120M$LANDFIRE_Class<-ifelse(C120M$LANDFIRE_Class=="", NA, C120M$LANDFIRE_Class)
C240M$LANDFIRE_Class<-ifelse(C240M$LANDFIRE_Class=="", NA, C240M$LANDFIRE_Class)

#####################################################
#Join Area Metric: 
#####################################################
setwd("C:/Users/matth/Desktop/Thesis Dev/Area_Metric")
Area_df<-read.csv("Area_df.csv")

#60
#Create AreaID
C60M$AreaID<-paste(C60M$Event_ID,C60M$DOB, sep = "_")
#Match AreaID
C60M$DOB_Area<- Area_df$Area..m.2.[match(C60M$AreaID, Area_df$AreaID, nomatch = NA_integer_)]
#120
#Create AreaID
C120M$AreaID<-paste(C120M$Event_ID,C120M$DOB, sep = "_")
#Match AreaID
C120M$DOB_Area<- Area_df$Area..m.2.[match(C120M$AreaID, Area_df$AreaID)]

#240
#Create AreaID
C240M$AreaID<-paste(C240M$Event_ID,C240M$DOB, sep = "_")
#Match AreaID
C240M$DOB_Area<- Area_df$Area..m.2.[match(C240M$AreaID, Area_df$AreaID)]

#Join area to the each internal points matched counterpart: 
#60
C60M$DOB_Area<-as.numeric(C60M$DOB_Area)
C60M$DOB_Area<-replace_na(C60M$DOB_Area, 0)

C60M<- C60M %>%mutate(DOB_Area = replace(DOB_Area, which(DOB_Area != "0") - 1, NA))  %>% as.data.frame()

C60M <-C60M %>%
  fill(DOB_Area, .direction = "up")  %>%
  as.data.frame()

C60M<- C60M %>%mutate(DOB_Area = replace(DOB_Area, which(DOB_Area == "0"), NA))  %>% as.data.frame()

#120
C120M$DOB_Area<-as.numeric(C120M$DOB_Area)
C120M$DOB_Area<-replace_na(C120M$DOB_Area, 0)

C120M<- C120M %>%mutate(DOB_Area = replace(DOB_Area, which(DOB_Area != "0") - 1, NA))  %>% as.data.frame()

C120M <-C120M %>%
  fill(DOB_Area, .direction = "up")  %>%
  as.data.frame()

C120M<- C120M %>%mutate(DOB_Area = replace(DOB_Area, which(DOB_Area == "0"), NA))  %>% as.data.frame()
#240
C240M$DOB_Area<-as.numeric(C240M$DOB_Area)
C240M$DOB_Area<-replace_na(C240M$DOB_Area, 0)

C240M<- C240M %>%mutate(DOB_Area = replace(DOB_Area, which(DOB_Area != "0") - 1, NA))  %>% as.data.frame()

C240M <-C240M %>%
  fill(DOB_Area, .direction = "up")  %>%
  as.data.frame()

C240M<- C240M %>%mutate(DOB_Area = replace(DOB_Area, which(DOB_Area == "0"), NA))  %>% as.data.frame()

#Group landcover into Coop's Classes: 
setwd("C:/Users/matth/Desktop/Method_1_Analysis/Perimeter_Analysis_V4/Contributors")
Coop_Classes<-read.csv("Coop_Classes.csv")

#Reclassify: 
#SW
C60M$SWReGAP_LandcoverType <- Coop_Classes$Coop_Class_1SW[match(C60M$SWReGAP_Class, Coop_Classes$SWReGAP_Class)]

C120M$SWReGAP_LandcoverType <- Coop_Classes$Coop_Class_1SW[match(C120M$SWReGAP_Class, Coop_Classes$SWReGAP_Class)]

C240M$SWReGAP_LandcoverType <- Coop_Classes$Coop_Class_1SW[match(C240M$SWReGAP_Class, Coop_Classes$SWReGAP_Class)]

#LF
C60M$LANDFIRE_LandcoverType <- Coop_Classes$Coop_Class_1LF[match(C60M$LANDFIRE_Class, Coop_Classes$LF_Class)]

C120M$LANDFIRE_LandcoverType <- Coop_Classes$Coop_Class_1LF[match(C120M$LANDFIRE_Class, Coop_Classes$LF_Class)]

C240M$LANDFIRE_LandcoverType <- Coop_Classes$Coop_Class_1LF[match(C240M$LANDFIRE_Class, Coop_Classes$LF_Class)]


#Save Final Cleaned Dataframes: 
#60
fwrite(C60M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Sequential_MP\\Cleaned_Points\\C60M.csv", append= FALSE)
#120
fwrite(C120M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Sequential_MP\\Cleaned_Points\\C120M.csv", append= FALSE)
#240
fwrite(C240M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Sequential_MP\\Cleaned_Points\\C240M.csv", append= FALSE)


#Experimental section: 
SW_Class_Count_60<-as.data.frame(table(C60M$SWReGAP_Class))
SW_LandcoverType_Count<-as.data.frame(table(C60M$Yrs_Since_Fire))
sum(SW_LandcoverType_Count$Freq)
