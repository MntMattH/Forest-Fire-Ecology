#PA5_Subset_Analysis_V3
#Matt Harris (8/6/2023)

#Load Packages and Data: 
#Set WD: 
#setwd("C:/Users/matth/Desktop/Method_1_Analysis/PA_V5/Sequential_MP/Cleaned_Points")
#or
setwd("C:/Users/matth/Desktop/Method_1_Analysis/PA_V5/Sequential_MP/Aspen_Only/Cleaned_Points")

#Load Packages: 
library(survival)
library(MuMIn)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(AICcmodavg)


#Load data: 
C60M<-read.csv("C60M.csv")
C120M<-read.csv("C120M.csv")
C240M<-read.csv("C240M.csv")

#Ensure blanks are NA's so _disturbed_nonflammable is the intercept all landcover types are compared to. 
C60M<- C60M %>%mutate(SWReGAP_LandcoverType = replace(SWReGAP_LandcoverType, which(SWReGAP_LandcoverType == ""), NA))  %>% as.data.frame()
C60M<- C60M %>%mutate(LANDFIRE_LandcoverType = replace(LANDFIRE_LandcoverType, which(LANDFIRE_LandcoverType == ""), NA))  %>% as.data.frame()
C120M<- C120M %>%mutate(SWReGAP_LandcoverType = replace(SWReGAP_LandcoverType, which(SWReGAP_LandcoverType == ""), NA))  %>% as.data.frame()
C120M<- C120M %>%mutate(LANDFIRE_LandcoverType = replace(LANDFIRE_LandcoverType, which(LANDFIRE_LandcoverType == ""), NA))  %>% as.data.frame()
C240M<- C240M %>%mutate(SWReGAP_LandcoverType = replace(SWReGAP_LandcoverType, which(SWReGAP_LandcoverType == ""), NA))  %>% as.data.frame()
C240M<- C240M %>%mutate(LANDFIRE_LandcoverType = replace(LANDFIRE_LandcoverType, which(LANDFIRE_LandcoverType == ""), NA))  %>% as.data.frame()
#Do the same for LC type 2
C60M<- C60M %>%mutate(SWReGAP_LandcoverType2 = replace(SWReGAP_LandcoverType2, which(SWReGAP_LandcoverType2 == ""), NA))  %>% as.data.frame()
C60M<- C60M %>%mutate(LANDFIRE_LandcoverType2 = replace(LANDFIRE_LandcoverType2, which(LANDFIRE_LandcoverType2 == ""), NA))  %>% as.data.frame()
C120M<- C120M %>%mutate(SWReGAP_LandcoverType2 = replace(SWReGAP_LandcoverType2, which(SWReGAP_LandcoverType2 == ""), NA))  %>% as.data.frame()
C120M<- C120M %>%mutate(LANDFIRE_LandcoverType2 = replace(LANDFIRE_LandcoverType2, which(LANDFIRE_LandcoverType2 == ""), NA))  %>% as.data.frame()
C240M<- C240M %>%mutate(SWReGAP_LandcoverType2 = replace(SWReGAP_LandcoverType2, which(SWReGAP_LandcoverType2 == ""), NA))  %>% as.data.frame()
C240M<- C240M %>%mutate(LANDFIRE_LandcoverType2 = replace(LANDFIRE_LandcoverType2, which(LANDFIRE_LandcoverType2 == ""), NA))  %>% as.data.frame()

#First subset: Above or below median DOB_AREA: 
C60M.Median.Area<- median(C60M$DOB_Area, na.rm = T)
C120M.Median.Area<- median(C120M$DOB_Area, na.rm = T)
C240M.Median.Area<- median(C240M$DOB_Area, na.rm = T)

#Create Subsets: 
C60M.Blw.Med.Area <- subset(C60M, DOB_Area <= C60M.Median.Area)
C60M.Abv.Med.Area <- subset(C60M, DOB_Area >= C60M.Median.Area) 
C120M.Blw.Med.Area <- subset(C120M, DOB_Area <= C120M.Median.Area)
C120M.Abv.Med.Area <- subset(C120M, DOB_Area >= C120M.Median.Area) 
C240M.Blw.Med.Area <- subset(C240M, DOB_Area <= C240M.Median.Area)
C240M.Abv.Med.Area <- subset(C240M, DOB_Area >= C240M.Median.Area) 

#Run Model Selection within Subsets: 
#Below Median DOB_Area: 
Below.Med.Area<-list()
#Landcover Only: 
#60
Below.Med.Area[[1]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C60M.Blw.Med.Area)
#120
Below.Med.Area[[2]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C120M.Blw.Med.Area)
#240
Below.Med.Area[[3]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C240M.Blw.Med.Area)

#Landcover plus years since fire 10 years (binary): 
#60
Below.Med.Area[[4]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C60M.Blw.Med.Area)
#120
Below.Med.Area[[5]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C120M.Blw.Med.Area)
#240
Below.Med.Area[[6]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C240M.Blw.Med.Area)

#Landcover plus years since fire 20 years (binary): 
#60
Below.Med.Area[[7]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C60M.Blw.Med.Area)
#120
Below.Med.Area[[8]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C120M.Blw.Med.Area)
#240
Below.Med.Area[[9]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C240M.Blw.Med.Area)

#Set up for conditional logistic regressions with Landcover + Aridity + PrefireNDVI + Ruggedness + Slope+ years since fire (binary): 
#60
Below.Med.Area[[10]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs + strata(ID_Year), data = C60M.Blw.Med.Area)
#120
Below.Med.Area[[11]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity +  PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs + strata(ID_Year), data = C120M.Blw.Med.Area)
#240
Below.Med.Area[[12]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.Blw.Med.Area)

summary(Below.Med.Area[[12]])

modelnames= c(1:12) #Model names 

Below.Med.AreaTable= aictab(cand.set = Below.Med.Area, modnames = modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
Below.Med.AreaTable

M1<-AIC(Below.Med.Area[[1]])
M2<-AIC(Below.Med.Area[[2]])
M3<-AIC(Below.Med.Area[[3]])
M4<-AIC(Below.Med.Area[[4]])
M5<-AIC(Below.Med.Area[[5]])
M6<-AIC(Below.Med.Area[[6]])
M7<-AIC(Below.Med.Area[[7]])
M8<-AIC(Below.Med.Area[[8]])
M9<-AIC(Below.Med.Area[[9]])
M10<-AIC(Below.Med.Area[[10]])
M11<-AIC(Below.Med.Area[[11]])
M12<-AIC(Below.Med.Area[[12]])

Model_List<-list(Model_Number= seq(1:12), AIC = c(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12))

Blw_Tab<- data.frame(Model_List)

#Above Median DOB_Area 
Above.Med.Area<-list()
#Landcover Only: 
#60
Above.Med.Area[[1]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C60M.Abv.Med.Area)
#120
Above.Med.Area[[2]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C120M.Abv.Med.Area)
#240
Above.Med.Area[[3]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C240M.Abv.Med.Area)

#Landcover plus years since fire (binary): 
#60
Above.Med.Area[[4]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C60M.Abv.Med.Area)
#120
Above.Med.Area[[5]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C120M.Abv.Med.Area)
#240
Above.Med.Area[[6]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C240M.Abv.Med.Area)

#Landcover plus years since fire (binary): 
#60
Above.Med.Area[[7]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C60M.Abv.Med.Area)
#120
Above.Med.Area[[8]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C120M.Abv.Med.Area)
#240
Above.Med.Area[[9]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C240M.Abv.Med.Area)

#Set up for conditional logistic regressions with Landcover + Aridity + PrefireNDVI + Ruggedness + Slope+ years since fire (binary): 
#60
Above.Med.Area[[10]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.Abv.Med.Area)
#120
Above.Med.Area[[11]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity +  PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.Abv.Med.Area)
#240
Above.Med.Area[[12]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.Abv.Med.Area)

summary(Above.Med.Area[[12]])

modelnames= c(1:12) #Model names 

Above.Med.AreaTable= aictab(cand.set = Above.Med.Area, modnames = modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
Above.Med.AreaTable

M1<-AIC(Above.Med.Area[[1]])
M2<-AIC(Above.Med.Area[[2]])
M3<-AIC(Above.Med.Area[[3]])
M4<-AIC(Above.Med.Area[[4]])
M5<-AIC(Above.Med.Area[[5]])
M6<-AIC(Above.Med.Area[[6]])
M7<-AIC(Above.Med.Area[[7]])
M8<-AIC(Above.Med.Area[[8]])
M9<-AIC(Above.Med.Area[[9]])
M10<-AIC(Above.Med.Area[[10]])
M11<-AIC(Above.Med.Area[[11]])
M12<-AIC(Above.Med.Area[[12]])

Model_List<-list(Model_Number= seq(1:12), AIC = c(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12))

Abv_Tab<- data.frame(Model_List)


################################################################################
                          #Begin subset by FSC
################################################################################
#First subset: Above or below median Aridity: 
C60M.Median.Aridity<- median(C60M$Aridity, na.rm = T)
C120M.Median.Aridity<- median(C120M$Aridity, na.rm = T)
C240M.Median.Aridity<- median(C240M$Aridity, na.rm = T)

#Create Subsets: 
C60M.Blw.Med.Aridity <- subset(C60M, Aridity <= C60M.Median.Aridity)
C60M.Abv.Med.Aridity <- subset(C60M, Aridity >= C60M.Median.Aridity) 
C120M.Blw.Med.Aridity <- subset(C120M, Aridity <= C120M.Median.Aridity)
C120M.Abv.Med.Aridity <- subset(C120M, Aridity >= C120M.Median.Aridity) 
C240M.Blw.Med.Aridity <- subset(C240M, Aridity <= C240M.Median.Aridity)
C240M.Abv.Med.Aridity <- subset(C240M, Aridity >= C240M.Median.Aridity) 

#Run Model Selection within Subsets: 
#Below Median DOB_Aridity: 
Below.Med.Aridity<-list()
#Landcover Only: 
#60
Below.Med.Aridity[[1]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C60M.Blw.Med.Aridity)
#120
Below.Med.Aridity[[2]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C120M.Blw.Med.Aridity)
#240
Below.Med.Aridity[[3]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C240M.Blw.Med.Aridity)

#Landcover plus years since fire 10yrs(binary): 
#60
Below.Med.Aridity[[4]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C60M.Blw.Med.Aridity)
#120
Below.Med.Aridity[[5]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C120M.Blw.Med.Aridity)
#240
Below.Med.Aridity[[6]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C240M.Blw.Med.Aridity)

#Landcover plus years since fire 20yrs(binary): 
#60
Below.Med.Aridity[[7]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_20yrs  + strata(ID_Year), data = C60M.Blw.Med.Aridity)
#120
Below.Med.Aridity[[8]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_20yrs  + strata(ID_Year), data = C120M.Blw.Med.Aridity)
#240
Below.Med.Aridity[[9]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_20yrs  + strata(ID_Year), data = C240M.Blw.Med.Aridity)

#Set up for conditional logistic regressions with Landcover  + PrefireNDVI + Ruggedness + Slope+ years since fire (binary): 
#60
Below.Med.Aridity[[10]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.Blw.Med.Aridity)
#120
Below.Med.Aridity[[11]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 +  PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.Blw.Med.Aridity)
#240
Below.Med.Aridity[[12]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.Blw.Med.Aridity)

summary(Below.Med.Aridity[[12]])

modelnames= c(1:12) #Model names 

Below.Med.AridityTable= aictab(cand.set = Below.Med.Aridity, modnames = modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
Below.Med.AridityTable


M1<-AIC(Below.Med.Aridity[[1]])
M2<-AIC(Below.Med.Aridity[[2]])
M3<-AIC(Below.Med.Aridity[[3]])
M4<-AIC(Below.Med.Aridity[[4]])
M5<-AIC(Below.Med.Aridity[[5]])
M6<-AIC(Below.Med.Aridity[[6]])
M7<-AIC(Below.Med.Aridity[[7]])
M8<-AIC(Below.Med.Aridity[[8]])
M9<-AIC(Below.Med.Aridity[[9]])
M10<-AIC(Below.Med.Aridity[[10]])
M11<-AIC(Below.Med.Aridity[[11]])
M12<-AIC(Below.Med.Aridity[[12]])

Model_List<-list(Model_Number= seq(1:12), AIC = c(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12))

Blw_Tab<- data.frame(Model_List)

#Above Median Aridity 
Above.Med.Aridity<-list()
#Landcover Only: 
#60
Above.Med.Aridity[[1]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C60M.Abv.Med.Aridity)
#120
Above.Med.Aridity[[2]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C120M.Abv.Med.Aridity)
#240
Above.Med.Aridity[[3]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C240M.Abv.Med.Aridity)

#Landcover plus years since fire 10yrs(binary): 
#60
Above.Med.Aridity[[4]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C60M.Abv.Med.Aridity)
#120
Above.Med.Aridity[[5]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C120M.Abv.Med.Aridity)
#240
Above.Med.Aridity[[6]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C240M.Abv.Med.Aridity)

#Landcover plus years since fire 20yrs(binary): 
#60
Above.Med.Aridity[[7]]<- clogit(Burned ~ LANDFIRE_LandcoverType2  + BWI_20yrs + strata(ID_Year), data = C60M.Abv.Med.Aridity)
#120
Above.Med.Aridity[[8]]<- clogit(Burned ~ LANDFIRE_LandcoverType2  + BWI_20yrs + strata(ID_Year), data = C120M.Abv.Med.Aridity)
#240
Above.Med.Aridity[[9]]<- clogit(Burned ~ LANDFIRE_LandcoverType2  + BWI_20yrs + strata(ID_Year), data = C240M.Abv.Med.Aridity)

#Set up for conditional logistic regressions with Landcover  + PrefireNDVI + Ruggedness + Slope+ years since fire (binary): 
#60
Above.Med.Aridity[[10]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.Abv.Med.Aridity)
#120
Above.Med.Aridity[[11]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 +  PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.Abv.Med.Aridity)
#240
Above.Med.Aridity[[12]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.Abv.Med.Aridity)

summary(Above.Med.Aridity[[12]])

modelnames= c(1:12) #Model names 

Above.Med.AridityTable= aictab(cand.set = Above.Med.Aridity, modnames = modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
Above.Med.AridityTable

M1<-AIC(Above.Med.Aridity[[1]])
M2<-AIC(Above.Med.Aridity[[2]])
M3<-AIC(Above.Med.Aridity[[3]])
M4<-AIC(Above.Med.Aridity[[4]])
M5<-AIC(Above.Med.Aridity[[5]])
M6<-AIC(Above.Med.Aridity[[6]])
M7<-AIC(Above.Med.Aridity[[7]])
M8<-AIC(Above.Med.Aridity[[8]])
M9<-AIC(Above.Med.Aridity[[9]])
M10<-AIC(Above.Med.Aridity[[10]])
M11<-AIC(Above.Med.Aridity[[11]])
M12<-AIC(Above.Med.Aridity[[12]])

Model_List<-list(Model_Number= seq(1:12), AIC = c(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12))

Abv_Tab<- data.frame(Model_List)

#Rework Standard Model: 
Standard_Model<-list()
#Landcover Only: 
#60
Standard_Model[[1]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + strata(ID_Year), data = C60M)
#120
Standard_Model[[2]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + strata(ID_Year), data = C120M)
#240
Standard_Model[[3]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + strata(ID_Year), data = C240M)

#Landcover plus years since fire (binary): 
#60
Standard_Model[[4]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C60M)
#120
Standard_Model[[5]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C120M)
#240
Standard_Model[[6]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + BWI_10yrs + strata(ID_Year), data = C240M)

#Landcover plus years since fire (binary): 
#60
Standard_Model[[7]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + BWI_20yrs + strata(ID_Year), data = C60M)
#120
Standard_Model[[8]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + BWI_20yrs + strata(ID_Year), data = C120M)
#240
Standard_Model[[9]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + BWI_20yrs + strata(ID_Year), data = C240M)


#Set up for conditional logistic regressions with Landcover + Aridity + PrefireNDVI + Ruggedness + Slope+ years since fire (binary): 
#60
Standard_Model[[10]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs + strata(ID_Year), data = C60M)
#120
Standard_Model[[11]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Aridity +  PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs + strata(ID_Year), data = C120M)
#240
Standard_Model[[12]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs + strata(ID_Year), data = C240M)

summary(Standard_Model[[6]])

M1<-AIC(Standard_Model[[1]])
M2<-AIC(Standard_Model[[2]])
M3<-AIC(Standard_Model[[3]])
M4<-AIC(Standard_Model[[4]])
M5<-AIC(Standard_Model[[5]])
M6<-AIC(Standard_Model[[6]])
M7<-AIC(Standard_Model[[7]])
M8<-AIC(Standard_Model[[8]])
M9<-AIC(Standard_Model[[9]])
M10<-AIC(Standard_Model[[10]])
M11<-AIC(Standard_Model[[11]])
M12<-AIC(Standard_Model[[12]])

Model_List<-list(Model_Number= seq(1:12), AIC = c(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12))

SM_Tab<- data.frame(Model_List)