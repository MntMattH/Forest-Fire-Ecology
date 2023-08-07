#Create binary columns based on years since fire then subset the data based on DOB_Area: 
#Matt Harris 7/26/2023
#Intent: determine if years since fire (0-10 and 0-20) has a significant influence on burn occurrence. Inquire about the differences presented by subsetting the data to: a) all data, b) all the data that burned up to the 84th percentile, c) above the 84%, and d) up to the 97.5% and e) >97.5% based on DOB_area. 

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

#First make a binary column for years since fire representing 0-10 years or not and 0-20 years or not. 
C60M$BWI_10yrs<- NA
C120M$BWI_10yrs<- NA
C240M$BWI_10yrs<- NA

C60M$BWI_20yrs<- NA
C120M$BWI_20yrs<- NA
C240M$BWI_20yrs<- NA
#Within ten years: 
C60M<- C60M %>%mutate(BWI_10yrs = replace(BWI_10yrs, which(Yrs_Since_Fire <= 10), 1))  %>% as.data.frame()
C60M<- C60M %>%mutate(BWI_10yrs = replace(BWI_10yrs, which(Yrs_Since_Fire > 10), 0))  %>% as.data.frame()
C120M<- C120M %>%mutate(BWI_10yrs = replace(BWI_10yrs, which(Yrs_Since_Fire <= 10), 1))  %>% as.data.frame()
C120M<- C120M %>%mutate(BWI_10yrs = replace(BWI_10yrs, which(Yrs_Since_Fire > 10), 0))  %>% as.data.frame()
C240M<- C240M %>%mutate(BWI_10yrs = replace(BWI_10yrs, which(Yrs_Since_Fire <= 10), 1))  %>% as.data.frame()
C240M<- C240M %>%mutate(BWI_10yrs = replace(BWI_10yrs, which(Yrs_Since_Fire > 10), 0))  %>% as.data.frame()

#Within twenty years: 
C60M<- C60M %>%mutate(BWI_20yrs = replace(BWI_20yrs, which(Yrs_Since_Fire <= 20), 1))  %>% as.data.frame()
C60M<- C60M %>%mutate(BWI_20yrs = replace(BWI_20yrs, which(Yrs_Since_Fire > 20), 0))  %>% as.data.frame()
C120M<- C120M %>%mutate(BWI_20yrs = replace(BWI_20yrs, which(Yrs_Since_Fire <= 20), 1))  %>% as.data.frame()
C120M<- C120M %>%mutate(BWI_20yrs = replace(BWI_20yrs, which(Yrs_Since_Fire > 20), 0))  %>% as.data.frame()
C240M<- C240M %>%mutate(BWI_20yrs = replace(BWI_20yrs, which(Yrs_Since_Fire <= 20), 1))  %>% as.data.frame()
C240M<- C240M %>%mutate(BWI_20yrs = replace(BWI_20yrs, which(Yrs_Since_Fire > 20), 0))  %>% as.data.frame()

#NAs to 0 for YSF:
C60M<- C60M %>%mutate(BWI_10yrs = replace(BWI_10yrs, is.na(BWI_10yrs), 0))  %>% as.data.frame()
C60M<- C60M %>%mutate(BWI_20yrs = replace(BWI_20yrs, is.na(BWI_20yrs), 0))  %>% as.data.frame()
C120M<- C120M %>%mutate(BWI_10yrs = replace(BWI_10yrs, is.na(BWI_10yrs), 0))  %>% as.data.frame()
C120M<- C120M %>%mutate(BWI_20yrs = replace(BWI_20yrs, is.na(BWI_20yrs), 0))  %>% as.data.frame()
C240M<- C240M %>%mutate(BWI_10yrs = replace(BWI_10yrs, is.na(BWI_10yrs), 0))  %>% as.data.frame()
C240M<- C240M %>%mutate(BWI_20yrs = replace(BWI_20yrs, is.na(BWI_20yrs), 0))  %>% as.data.frame()

#Random Points: 
RP$BWI_10yrs<- NA
RP$BWI_20yrs<- NA
RP<- RP %>%mutate(BWI_10yrs = replace(BWI_10yrs, which(Yrs_Since_Fire <= 10), 1))  %>% as.data.frame()
RP<- RP %>%mutate(BWI_10yrs = replace(BWI_10yrs, which(Yrs_Since_Fire > 10), 0))  %>% as.data.frame()
RP<- RP %>%mutate(BWI_20yrs = replace(BWI_20yrs, which(Yrs_Since_Fire <= 20), 1))  %>% as.data.frame()
RP<- RP %>%mutate(BWI_20yrs = replace(BWI_20yrs, which(Yrs_Since_Fire > 20), 0))  %>% as.data.frame()
RP<- RP %>%mutate(BWI_10yrs = replace(BWI_10yrs, is.na(BWI_10yrs), 0))  %>% as.data.frame()
RP<- RP %>%mutate(BWI_20yrs = replace(BWI_20yrs, is.na(BWI_20yrs), 0))  %>% as.data.frame()

library(data.table)
#Save new YSF binary metrics:
# #60
# fwrite(C60M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\PA_V5\\Sequential_MP\\Aspen_Only\\Cleaned_Points\\C60M.csv", append= FALSE)
# #120
# fwrite(C120M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\PA_V5\\Sequential_MP\\Aspen_Only\\Cleaned_Points\\C120M.csv", append= FALSE)
# #240
# fwrite(C240M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\PA_V5\\Sequential_MP\\Aspen_Only\\Cleaned_Points\\C240M.csv", append= FALSE)

# #Write all:
# fwrite(C60M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\PA_V5\\Sequential_MP\\Cleaned_Points\\C60M.csv", append= FALSE)
# #120
# fwrite(C120M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\PA_V5\\Sequential_MP\\Cleaned_Points\\C120M.csv", append= FALSE)
# #240
# fwrite(C240M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\PA_V5\\Sequential_MP\\Cleaned_Points\\C240M.csv", append= FALSE)

#Begin subsetting based on DOB_Area: (all data,  <= 84%, > 84%, and d) 84%- 97.5% and e) >97.5% based on DOB_area. 

#Code from Jess for extreme spread: (edited to include above subsets)

#Create thresholds:
#60
## mean + 1SD (84th percentile)
C60M.ExtremeThreshold.AREA.1SD <- exp(mean(log(C60M$DOB_Area), na.rm=T) + sd(log(C60M$DOB_Area), na.rm = T)) 
## mean + 2SD (94th percentile)
C60M.ExtremeThreshold.AREA.2SD <- exp(mean(log(C60M$DOB_Area), na.rm = T) + sd(log(C60M$DOB_Area), na.rm = T)*2) 
#120
## mean + 1SD (84th percentile)
C120M.ExtremeThreshold.AREA.1SD <- exp(mean(log(C120M$DOB_Area), na.rm = T) + sd(log(C120M$DOB_Area), na.rm = T)) 
## mean + 2SD (94th percentile)
C120M.ExtremeThreshold.AREA.2SD <- exp(mean(log(C120M$DOB_Area), na.rm = T) + sd(log(C120M$DOB_Area), na.rm = T)*2) 
#240
## mean + 1SD (84th percentile)
C240M.ExtremeThreshold.AREA.1SD <- exp(mean(log(C240M$DOB_Area), na.rm = T) + sd(log(C240M$DOB_Area), na.rm = T)) 
## mean + 2SD (94th percentile)
C240M.ExtremeThreshold.AREA.2SD <- exp(mean(log(C240M$DOB_Area), na.rm = T) + sd(log(C240M$DOB_Area), na.rm = T)*2) 

#Subset by thresholds:
#Common Spread Day (CSD)
#60
C60M.CSD <- subset(C60M, DOB_Area <= C60M.ExtremeThreshold.AREA.1SD)
C60M.EXSD.1SD <- subset(C60M, DOB_Area >= C60M.ExtremeThreshold.AREA.1SD) 
C60M.EXSD.1to2SD <- subset(C60M, C60M.ExtremeThreshold.AREA.1SD < DOB_Area & DOB_Area < C60M.ExtremeThreshold.AREA.2SD)
C60M.EXSD.2SD <- subset(C60M, DOB_Area >= C60M.ExtremeThreshold.AREA.2SD)
#120
C120M.CSD <- subset(C120M, DOB_Area <= C120M.ExtremeThreshold.AREA.1SD)
C120M.EXSD.1SD <- subset(C120M, DOB_Area >= C120M.ExtremeThreshold.AREA.1SD) 
C120M.EXSD.1to2SD <- subset(C120M, C120M.ExtremeThreshold.AREA.1SD < DOB_Area & DOB_Area < C120M.ExtremeThreshold.AREA.2SD)
C120M.EXSD.2SD <- subset(C120M, DOB_Area >= C120M.ExtremeThreshold.AREA.2SD)
#240
C240M.CSD <- subset(C240M, DOB_Area <= C240M.ExtremeThreshold.AREA.1SD)
C240M.EXSD.1SD <- subset(C240M, DOB_Area >= C240M.ExtremeThreshold.AREA.1SD) 
C240M.EXSD.1to2SD <- subset(C240M, C240M.ExtremeThreshold.AREA.1SD < DOB_Area & DOB_Area < C240M.ExtremeThreshold.AREA.2SD)
C240M.EXSD.2SD <- subset(C240M, DOB_Area >= C240M.ExtremeThreshold.AREA.2SD)

#Now run tests on all data and new subsets: 
#Run slough of regressions on subsets to compare differences: (Use a find and replace to change between landcover datasets)
#All data:
Standard_Model<-list()
#Landcover Only: 
#60
Standard_Model[[1]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C60M)
#120
Standard_Model[[2]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C120M)
#240
Standard_Model[[3]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C240M)

#Landcover plus years since fire (binary): 
#60
Standard_Model[[4]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C60M)
#120
Standard_Model[[5]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C120M)
#240
Standard_Model[[6]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + PrefireNDVI + Ruggedness + Slope + years since fire (binary): 
#60
Standard_Model[[7]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M)
#120
Standard_Model[[8]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M)
#240
Standard_Model[[9]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Aridity + PrefireNDVI + Ruggedness + Slope+ years since fire (binary): 
#60
Standard_Model[[10]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M)
#120
Standard_Model[[11]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity +  PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M)
#240
Standard_Model[[12]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M)

summary(Standard_Model[[1]])

modelnames= c(1:12) #Model names 

Standard_ModelTable= aictab(cand.set = Standard_Model, modnames = modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
Standard_ModelTable
################################################
#Common Spread Day: 
CSD_Model<-list()
#Landcover Only: 
#60
CSD_Model[[1]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C60M.CSD)
#120
CSD_Model[[2]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C120M.CSD)
#240
CSD_Model[[3]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C240M.CSD)

#Landcover plus years since fire (binary): 
#60
CSD_Model[[4]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C60M.CSD)
#120
CSD_Model[[5]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C120M.CSD)
#240
CSD_Model[[6]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C240M.CSD)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + PrefireNDVI + Ruggedness + Slope + years since fire (binary): 
#60
CSD_Model[[7]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.CSD)
#120
CSD_Model[[8]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.CSD)
#240
CSD_Model[[9]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.CSD)

#Set up for conditional logistic regressions with Landcover + Aridity + PrefireNDVI + Ruggedness + Slope+ years since fire (binary): 
#60
CSD_Model[[10]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.CSD)
#120
CSD_Model[[11]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity +  PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.CSD)
#240
CSD_Model[[12]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.CSD)

summary(CSD_Model[[12]])

modelnames= c(1:12) #Model names 

CSD_ModelTable= aictab(cand.set = CSD_Model, modnames = modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
CSD_ModelTable
################################################
#Large Spread Day: 
OneSD_Model<-list()
#Landcover Only: 
#60
OneSD_Model[[1]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C60M.EXSD.1SD)
#120
OneSD_Model[[2]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C120M.EXSD.1SD)
#240
OneSD_Model[[3]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C240M.EXSD.1SD)

#Landcover plus years since fire (binary): 
#60
OneSD_Model[[4]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C60M.EXSD.1SD)
#120
OneSD_Model[[5]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C120M.EXSD.1SD)
#240
OneSD_Model[[6]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C240M.EXSD.1SD)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + PrefireNDVI + Ruggedness + Slope + years since fire (binary): 
#60
OneSD_Model[[7]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.EXSD.1SD)
#120
OneSD_Model[[8]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.EXSD.1SD)
#240
OneSD_Model[[9]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.EXSD.1SD)

#Set up for conditional logistic regressions with Landcover + Aridity + PrefireNDVI + Ruggedness + Slope+ years since fire (binary): 
#60
OneSD_Model[[10]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.EXSD.1SD)
#120
OneSD_Model[[11]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity +  PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.EXSD.1SD)
#240
OneSD_Model[[12]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.EXSD.1SD)

summary(OneSD_Model[[1]])

modelnames= c(1:12) #Model names 

OneSD_ModelTable= aictab(cand.set = OneSD_Model, modnames = modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
OneSD_ModelTable
################################################
#Large Spread Day: 
LSD_Model<-list()
#Landcover Only: 
#60
LSD_Model[[1]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C60M.EXSD.1to2SD)
#120
LSD_Model[[2]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C120M.EXSD.1to2SD)
#240
LSD_Model[[3]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C240M.EXSD.1to2SD)

#Landcover plus years since fire (binary): 
#60
LSD_Model[[4]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C60M.EXSD.1to2SD)
#120
LSD_Model[[5]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C120M.EXSD.1to2SD)
#240
LSD_Model[[6]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C240M.EXSD.1to2SD)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + PrefireNDVI + Ruggedness + Slope + years since fire (binary): 
#60
LSD_Model[[7]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.EXSD.1to2SD)
#120
LSD_Model[[8]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.EXSD.1to2SD)
#240
LSD_Model[[9]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.EXSD.1to2SD)

#Set up for conditional logistic regressions with Landcover + Aridity + PrefireNDVI + Ruggedness + Slope+ years since fire (binary): 
#60
LSD_Model[[10]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.EXSD.1to2SD)
#120
LSD_Model[[11]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity +  PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.EXSD.1to2SD)
#240
LSD_Model[[12]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.EXSD.1to2SD)

summary(LSD_Model[[12]])

modelnames= c(1:12) #Model names 

LSD_ModelTable= aictab(cand.set = LSD_Model, modnames = modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
LSD_ModelTable
################################################
#Extreme Spread Day: 
ESD_Model<-list()
#Landcover Only: 
#60
ESD_Model[[1]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C60M.EXSD.2SD)
#120
ESD_Model[[2]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C120M.EXSD.2SD)
#240
ESD_Model[[3]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C240M.EXSD.2SD)

#Landcover plus years since fire (binary): 
#60
ESD_Model[[4]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C60M.EXSD.2SD)
#120
ESD_Model[[5]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C120M.EXSD.2SD)
#240
ESD_Model[[6]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + BWI_10yrs  + strata(ID_Year), data = C240M.EXSD.2SD)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + PrefireNDVI + Ruggedness + Slope + years since fire (binary): 
#60
ESD_Model[[7]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.EXSD.2SD)
#120
ESD_Model[[8]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.EXSD.2SD)
#240
ESD_Model[[9]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.EXSD.2SD)

#Set up for conditional logistic regressions with Landcover + Aridity + PrefireNDVI + Ruggedness + Slope+ years since fire (binary): 
#60
ESD_Model[[10]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C60M.EXSD.2SD)
#120
ESD_Model[[11]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity +  PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C120M.EXSD.2SD)
#240
ESD_Model[[12]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + BWI_10yrs  + strata(ID_Year), data = C240M.EXSD.2SD)

summary(ESD_Model[[10]])

modelnames= c(1:12) #Model names 

ESD_ModelTable= aictab(cand.set = ESD_Model, modnames = modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
ESD_ModelTable


