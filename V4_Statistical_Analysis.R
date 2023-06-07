#Perimeter Analysis V4: Statistical Analysis (Begin Un-grouped)
#Matt Harris 
#5/29/2023

#Set WD: 
setwd("C:/Users/matth/Desktop/Method_1_Analysis/Perimeter_Analysis_V4/Cleaned_Points")

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
C60M<-read.csv("Clean_MP_60m.csv")
C120M<-read.csv("Clean_MP_120m.csv")
C240M<-read.csv("Clean_MP_240m.csv")


#Write script intro: 

#Begin models: conditional logistic regressions 

SW_Models=list() #create a empty list of models.

################################################################################SWReGAP Models:
#Set up for conditional logistic regressions with SWReGAP 
#60
#Models_60
clogit_60_SW <- clogit(Burned ~ SWReGAP_Class + strata(ORIG_FID), data = C60M)
summary(clogit_60_SW)
SW_Models[[1]]<- clogit(Burned ~ SWReGAP_Class + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_SW <- clogit(Burned ~ SWReGAP_Class + strata(ORIG_FID), data = C120M)
#summary(clogit_120_SW)
SW_Models[[2]]<- clogit(Burned ~ SWReGAP_Class + strata(ORIG_FID), data = C120M)

#240
#Models_240
#clogit_240_SW <- clogit(Burned ~ SWReGAP_Class + strata(ORIG_FID), data = C240M)
#summary(clogit_240_SW)
SW_Models[[3]]<- clogit(Burned ~ SWReGAP_Class + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables: 
#60
#Models_60
#clogit_60_LC_FSC <- clogit(Burned ~ SWReGAP_Class  * vpd_dsf * def_dsf * Tmax_dsf + strata(ORIG_FID), data = C60M)
#summary(clogit_60_LC_FSC)
SW_Models[[4]]<- clogit(Burned ~ SWReGAP_Class  * vpd_dsf * def_dsf * Tmax_dsf + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_LC_FSC <-
#summary(clogit_120_LC_FSC)
SW_Models[[5]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + strata(ORIG_FID), data = C120M)

#240
#Models_240
#clogit_240_LC_FSC <- 
#summary(clogit_240_LC_FSC)
SW_Models[[6]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + Years Since Fire: 
#60
#Models_60
#clogit_60_LC_FSC_PB <- 
#summary(clogit_60_LC_FSC_PB)
SW_Models[[7]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_LC_FSC_PB <- 
#summary(clogit_120_LC_FSC_PB)
SW_Models[[8]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + strata(ORIG_FID), data = C120M)

#240
#Models_240
#clogit_240_LC_FSC_PB <- 
#summary(clogit_240_LC_FSC_PB)
SW_Models[[9]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + Years Since Fire + PrefireNDVI: 
#60
#Models_60
#clogit_60_LC_FSC_PB_PF <- 
#summary(clogit_60_LC_FSC_PB_PF)
SW_Models[[10]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_LC_FSC_PB_PF <- 
#summary(clogit_120_LC_FSC_PB_PF)
SW_Models[[11]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + strata(ORIG_FID), data = C120M)

#240
#Models_240
#clogit_240_LC_FSC_PB_PF <- 
#summary(clogit_240_LC_FSC_PB_PF)
SW_Models[[12]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + Years Since Fire + PrefireNDVI + Ruggedness: 
#60
#Models_60
#clogit_60_LC_FSC_PB_PF_R <- 
#summary(clogit_60_LC_FSC_PB_PF_R)
SW_Models[[13]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_LC_FSC_PB_PF_R <- 
#summary(clogit_120_LC_FSC_PB_PF_R)
SW_Models[[14]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + strata(ORIG_FID), data = C120M)

#240
#Models_240
#clogit_240_LC_FSC_PB_PF_R <- 
#summary(clogit_240_LC_FSC_PB_PF_R)
SW_Models[[15]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + Years Since Fire + PrefireNDVI + Ruggedness + Slope: 
#60
#Models_60
#clogit_60_All <- 
#summary(clogit_60_All)
SW_Models[[16]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_All <- 
#summary(clogit_120_All)
SW_Models[[17]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C120M)

#240
#Models_240
#All_Models
#clogit_240_All <- 
#summary(clogit_240_All)
SW_Models[[18]]<- clogit(Burned ~ SWReGAP_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Aridity + Years Since Fire + PrefireNDVI + Ruggedness + Slope: 
#60
#Models_60
#clogit_60_All <- 
#summary(clogit_60_All)
SW_Models[[19]]<- clogit(Burned ~ SWReGAP_Class * Aridity + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_All <- 
#summary(clogit_120_All)
SW_Models[[20]]<- clogit(Burned ~ SWReGAP_Class * Aridity + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C120M)

#240
#Models_240
#All_Models
#clogit_240_All <- 
#summary(clogit_240_All)
SW_Models[[21]]<- clogit(Burned ~ SWReGAP_Class * Aridity + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C240M)

summary(SW_Models[[3]])


#Begin Model selection Process: 

SW_modelnames= c() #Model names 

SW_modeltable= aictab(cand.set = SW_Models, modnames = SW_modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
SW_modeltable

?clogit

################################################################################LANDFIRE Models: 

LF_Models=list() #create a empty list of models.

#Set up for conditional logistic regressions with LANDFIRE
#60
#Models_60
clogit_60_LF <- clogit(Burned ~ LANDFIRE_Class + strata(ORIG_FID), data = C60M)
summary(clogit_60_LF)
LF_Models[[1]]<- clogit(Burned ~ LANDFIRE_Class + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_LF <- clogit(Burned ~ LANDFIRE_Class + strata(ORIG_FID), data = C120M)
#summary(clogit_120_LF)
LF_Models[[2]]<- clogit(Burned ~ LANDFIRE_Class + strata(ORIG_FID), data = C120M)

#240
#Models_240
#clogit_240_LF <- clogit(Burned ~ LANDFIRE_Class + strata(ORIG_FID), data = C240M)
#summary(clogit_240_LF)
LF_Models[[3]]<- clogit(Burned ~ LANDFIRE_Class + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables: 
#60
#Models_60
#clogit_60_LC_FSC <- 
#summary(clogit_60_LC_FSC)
LF_Models[[4]]<- clogit(Burned ~ LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_LC_FSC <- 
#summary(clogit_120_LC_FSC)
LF_Models[[5]]<- clogit(Burned ~  LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + strata(ORIG_FID), data = C120M)

#240
#Models_240
#clogit_240_LC_FSC <- 
#summary(clogit_240_LC_FSC)
LF_Models[[6]]<- clogit(Burned ~  LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + Years Since Fire: 
#60
#Models_60
#clogit_60_LC_FSC_PB <- 
#summary(clogit_60_LC_FSC_PB)
LF_Models[[7]]<- clogit(Burned ~  LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_LC_FSC_PB <- 
#summary(clogit_120_LC_FSC_PB)
LF_Models[[8]]<- clogit(Burned ~  LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + strata(ORIG_FID), data = C120M)

#240
#Models_240
#clogit_240_LC_FSC_PB <- 
#summary(clogit_240_LC_FSC_PB)
LF_Models[[9]]<- clogit(Burned ~  LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + Years Since Fire + PrefireNDVI: 
#60
#Models_60
#clogit_60_LC_FSC_PB_PF <- 
#summary(clogit_60_LC_FSC_PB_PF)
LF_Models[[10]]<- clogit(Burned ~ LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_LC_FSC_PB_PF <-
#summary(clogit_120_LC_FSC_PB_PF)
LF_Models[[11]]<- clogit(Burned ~ LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + strata(ORIG_FID), data = C120M)

#240
#Models_240
#clogit_240_LC_FSC_PB_PF <- 
#summary(clogit_240_LC_FSC_PB_PF)
LF_Models[[12]]<- clogit(Burned ~ LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + Years Since Fire + PrefireNDVI + Ruggedness: 
#60
#Models_60
#clogit_60_LC_FSC_PB_PF_R <- 
#summary(clogit_60_LC_FSC_PB_PF_R)
LF_Models[[13]]<- clogit(Burned ~ LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_LC_FSC_PB_PF_R <- 
#summary(clogit_120_LC_FSC_PB_PF_R)
LF_Models[[14]]<- clogit(Burned ~ LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + strata(ORIG_FID), data = C120M)

#240
#Models_240
#clogit_240_LC_FSC_PB_PF_R <- 
#summary(clogit_240_LC_FSC_PB_PF_R)
LF_Models[[15]]<- clogit(Burned ~ LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + Years Since Fire + PrefireNDVI + Ruggedness + Slope: 
#60
#Models_60
#clogit_60_All <- 
#summary(clogit_60_All)
LF_Models[[16]]<- clogit(Burned ~ LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_All <- 
#summary(clogit_120_All)
LF_Models[[17]]<- clogit(Burned ~ LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C120M)

#240
#Models_240
#All_Models
#clogit_240_All <- 
#summary(clogit_240_All)
LF_Models[[18]]<- clogit(Burned ~ LANDFIRE_Class * vpd_dsf * def_dsf * Tmax_dsf + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C240M)

#Set up for conditional logistic regressions with Landcover + Aridity + Years Since Fire + PrefireNDVI + Ruggedness + Slope: 
#60
#Models_60
#clogit_60_All <- 
#summary(clogit_60_All)
LF_Models[[19]]<- clogit(Burned ~ LANDFIRE_Class * Aridity + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C60M)

#120
#Models_120
#clogit_120_All <- 
#summary(clogit_120_All)
LF_Models[[20]]<- clogit(Burned ~ LANDFIRE_Class * Aridity + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C120M)

#240
#Models_240
#All_Models
#clogit_240_All <- 
#summary(clogit_240_All)
LF_Models[[21]]<- clogit(Burned ~ LANDFIRE_Class * Aridity + Yrs_Since_Fire + PFNDVI_dsf + Ruggedness + Slope + strata(ORIG_FID), data = C240M)

summary(LF_Models[[3]])

#Begin Model selection Process: 

LF_modelnames= c() #Model names 

LF_modeltable= aictab(cand.set = LF_Models, modnames = LF_modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
LF_modeltable

################################################################################
#Begin sub-setting the FSC data into 10 year ranges for above average fire season climate and nominal or below average fire season. #The aridity metric may make this unnecessary, but suffers the same spatial correspondence as the three FSC variables. 


#Step 1. Make a subset of averages above and below mean.
#Step 2. Take and equivalent random subset of the new subsets making two distinct data frames. 
#Step 3. Compare these data frames using the same tests as above to see if there is a notable difference in trends by FSC 

mean_vpd<-mean(C60M$vpd_dsf)

mean_def<-mean(C60M$def_dsf)

mean_Tmax<-mean(C60M$Tmax_dsf)

#Step 1.
#60
GT_vpd_60<- subset(C60M, C60M$vpd_dsf  > mean_vpd)
LT_vpd_60<- subset(C60M, C60M$vpd_dsf  < mean_vpd)

GT_def_60<- subset(C60M, C60M$def_dsf > mean_def)
LT_def_60<- subset(C60M, C60M$def_dsf <  mean_def)

GT_Tmax_60<- subset(C60M, C60M$Tmax_dsf > mean_Tmax)
LT_Tmax_60<- subset(C60M, C60M$Tmax_dsf <  mean_Tmax)

#120
GT_vpd_120<- subset(C120M, C120M$vpd_dsf  > mean_vpd)
LT_vpd_120<- subset(C120M, C120M$vpd_dsf  < mean_vpd)

GT_def_120<- subset(C120M, C120M$def_dsf > mean_def)
LT_def_120<- subset(C120M, C120M$def_dsf <  mean_def)

GT_Tmax_120<- subset(C120M, C120M$Tmax_dsf > mean_Tmax)
LT_Tmax_120<- subset(C120M, C120M$Tmax_dsf <  mean_Tmax)

#240
GT_vpd_240<- subset(C240M, C240M$vpd_dsf  > mean_vpd)
LT_vpd_240<- subset(C240M, C240M$vpd_dsf  < mean_vpd)

GT_def_240<- subset(C240M, C240M$def_dsf > mean_def)
LT_def_240<- subset(C240M, C240M$def_dsf <  mean_def)

GT_Tmax_240<- subset(C240M, C240M$Tmax_dsf > mean_Tmax)
LT_Tmax_240<- subset(C240M, C240M$Tmax_dsf <  mean_Tmax)

#Step 2. 
#60
RS_GT_vpd_60<- sample_n(GT_vpd_60, 30000)
RS_LT_vpd_60<- sample_n(LT_vpd_60, 30000)

RS_GT_def_60<- sample_n(GT_def_60, 15000)
RS_LT_def_60<- sample_n(LT_def_60, 15000)

RS_GT_Tmax_60<- sample_n(GT_Tmax_60, 30000)
RS_LT_Tmax_60<- sample_n(LT_Tmax_60, 30000)

#120
RS_GT_vpd_120<- sample_n(GT_vpd_120, 30000)
RS_LT_vpd_120<- sample_n(LT_vpd_120, 30000)

RS_GT_def_120<- sample_n(GT_def_120, 15000)
RS_LT_def_120<- sample_n(LT_def_120, 15000)

RS_GT_Tmax_120<- sample_n(GT_Tmax_120, 30000)
RS_LT_Tmax_120<- sample_n(LT_Tmax_120, 30000)

#240
RS_GT_vpd_240<- sample_n(GT_vpd_240, 30000)
RS_LT_vpd_240<- sample_n(LT_vpd_240, 30000)

RS_GT_def_240<- sample_n(GT_def_240, 15000)
RS_LT_def_240<- sample_n(LT_def_240, 15000)

RS_GT_Tmax_240<- sample_n(GT_Tmax_240, 30000)
RS_LT_Tmax_240<- sample_n(LT_Tmax_240, 30000)

#Step 3: Set up tests for these data frames

#60 Landfire  
GT_vpd_60_LF <- clogit(Burned ~ LANDFIRE_dsf + vpd_dsf + strata(ORIG_FID), data = RS_GT_vpd_60)
GT_vpd_60_LF
LT_vpd_60_LF <- clogit(Burned ~ LANDFIRE_dsf + vpd_dsf + strata(ORIG_FID), data = RS_LT_vpd_60)
LT_vpd_60_LF
GT_def_60_LF <- clogit(Burned ~ LANDFIRE_dsf + def_dsf + strata(ORIG_FID), data = RS_GT_def_60)
GT_def_60_LF
LT_def_60_LF <- clogit(Burned ~ LANDFIRE_dsf + def_dsf + strata(ORIG_FID), data = RS_LT_def_60)
LT_def_60_LF
GT_Tmax_60_LF <- clogit(Burned ~ LANDFIRE_dsf + Tmax_dsf + strata(ORIG_FID), data = RS_GT_Tmax_60)
GT_Tmax_60_LF
LT_Tmax_60_LF <- clogit(Burned ~ LANDFIRE_dsf + Tmax_dsf + strata(ORIG_FID), data = RS_LT_Tmax_60)
LT_Tmax_60_LF
#60 SWReGAP
GT_vpd_60_SW <- clogit(Burned ~ SWReGAP_Class + vpd_dsf + strata(ORIG_FID), data = RS_GT_vpd_60)
GT_vpd_60_SW
LT_vpd_60_SW <- clogit(Burned ~ SWReGAP_Class + vpd_dsf + strata(ORIG_FID), data = RS_LT_vpd_60)
LT_vpd_60_SW
GT_def_60_SW <- clogit(Burned ~ SWReGAP_Class + def_dsf + strata(ORIG_FID), data = RS_GT_def_60)
GT_def_60_SW
LT_def_60_SW <- clogit(Burned ~ SWReGAP_Class + def_dsf + strata(ORIG_FID), data = RS_LT_def_60)
LT_def_60_SW
GT_Tmax_60_SW <- clogit(Burned ~ SWReGAP_Class + Tmax_dsf + strata(ORIG_FID), data = RS_GT_Tmax_60)
GT_Tmax_60_SW
LT_Tmax_60_SW <- clogit(Burned ~ SWReGAP_Class + Tmax_dsf + strata(ORIG_FID), data = RS_LT_Tmax_60)
LT_Tmax_60_SW

#120 Landfire  
GT_vpd_120_LF <- clogit(Burned ~ LANDFIRE_dsf + vpd_dsf + strata(ORIG_FID), data = RS_GT_vpd_120)
GT_vpd_120_LF
LT_vpd_120_LF <- clogit(Burned ~ LANDFIRE_dsf + vpd_dsf + strata(ORIG_FID), data = RS_LT_vpd_120)
LT_vpd_120_LF
GT_def_120_LF <- clogit(Burned ~ LANDFIRE_dsf + def_dsf + strata(ORIG_FID), data = RS_GT_def_120)
GT_def_120_LF
LT_def_120_LF <- clogit(Burned ~ LANDFIRE_dsf + def_dsf + strata(ORIG_FID), data = RS_LT_def_120)
LT_def_120_LF
GT_Tmax_120_LF <- clogit(Burned ~ LANDFIRE_dsf + Tmax_dsf + strata(ORIG_FID), data = RS_GT_Tmax_120)
GT_Tmax_120_LF
LT_Tmax_120_LF <- clogit(Burned ~ LANDFIRE_dsf + Tmax_dsf + strata(ORIG_FID), data = RS_LT_Tmax_120)
LT_Tmax_120_LF
#120 SWReGAP
GT_vpd_120_SW <- clogit(Burned ~ SWReGAP_Class + vpd_dsf + strata(ORIG_FID), data = RS_GT_vpd_120)
GT_vpd_120_SW
LT_vpd_120_SW <- clogit(Burned ~ SWReGAP_Class + vpd_dsf + strata(ORIG_FID), data = RS_LT_vpd_120)
LT_vpd_120_SW
GT_def_120_SW <- clogit(Burned ~ SWReGAP_Class + def_dsf + strata(ORIG_FID), data = RS_GT_def_120)
GT_def_120_SW
LT_def_120_SW <- clogit(Burned ~ SWReGAP_Class + def_dsf + strata(ORIG_FID), data = RS_LT_def_120)
LT_def_120_SW
GT_Tmax_120_SW <- clogit(Burned ~ SWReGAP_Class + Tmax_dsf + strata(ORIG_FID), data = RS_GT_Tmax_120)
GT_Tmax_120_SW
LT_Tmax_120_SW <- clogit(Burned ~ SWReGAP_Class + Tmax_dsf + strata(ORIG_FID), data = RS_LT_Tmax_120)
LT_Tmax_120_SW

#240 Landfire  
GT_vpd_240_LF <- clogit(Burned ~ LANDFIRE_dsf + vpd_dsf + strata(ORIG_FID), data = RS_GT_vpd_240)
GT_vpd_240_LF
LT_vpd_240_LF <- clogit(Burned ~ LANDFIRE_dsf + vpd_dsf + strata(ORIG_FID), data = RS_LT_vpd_240)
LT_vpd_240_LF
GT_def_240_LF <- clogit(Burned ~ LANDFIRE_dsf + def_dsf + strata(ORIG_FID), data = RS_GT_def_240)
GT_def_240_LF
LT_def_240_LF <- clogit(Burned ~ LANDFIRE_dsf + def_dsf + strata(ORIG_FID), data = RS_LT_def_240)
LT_def_240_LF
GT_Tmax_240_LF <- clogit(Burned ~ LANDFIRE_dsf + Tmax_dsf + strata(ORIG_FID), data = RS_GT_Tmax_240)
GT_Tmax_240_LF
LT_Tmax_240_LF <- clogit(Burned ~ LANDFIRE_dsf + Tmax_dsf + strata(ORIG_FID), data = RS_LT_Tmax_240)
LT_Tmax_240_LF
#240 SWReGAP
GT_vpd_240_SW <- clogit(Burned ~ SWReGAP_Class + vpd_dsf + strata(ORIG_FID), data = RS_GT_vpd_240)
GT_vpd_240_SW
LT_vpd_240_SW <- clogit(Burned ~ SWReGAP_Class + vpd_dsf + strata(ORIG_FID), data = RS_LT_vpd_240)
LT_vpd_240_SW
GT_def_240_SW <- clogit(Burned ~ SWReGAP_Class + def_dsf + strata(ORIG_FID), data = RS_GT_def_240)
GT_def_240_SW
LT_def_240_SW <- clogit(Burned ~ SWReGAP_Class + def_dsf + strata(ORIG_FID), data = RS_LT_def_240)
LT_def_240_SW
GT_Tmax_240_SW <- clogit(Burned ~ SWReGAP_Class + Tmax_dsf + strata(ORIG_FID), data = RS_GT_Tmax_240)
GT_Tmax_240_SW
LT_Tmax_240_SW <- clogit(Burned ~ SWReGAP_Class + Tmax_dsf + strata(ORIG_FID), data = RS_LT_Tmax_240)
LT_Tmax_240_SW


