#Perimeter Analysis V5: Statistical Analysis 
#Matt Harris 
#7/21/2023 (There have since been three versions of this script. To find the most up to date please visit my GitHub site: https://github.com/MntMattH/Forest-Fire-Ecology/tree/MntMattH_Perimeter_Analysis)

#Set WD: 
# setwd("C:/Users/matth/Desktop/Method_1_Analysis/Perimeter_Analysis_V4/Sequential_MP/Cleaned_Points")
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


#Introduction: The goal of this script is to conduct statistical analysis on matched pairs of points and random points generated for Matt Harris's fire perimeter analysis. The match points will be evaluated using a conditional logistic regression to compare covariates directly between each point in a matched pair. Random points will be evaluated using a logistic regression. A model selection process will be used to determine the best fit model for further examination. 


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

#Begin models: conditional logistic regressions 

SW_Models=list() #create a empty list of models.

################################################################################SWReGAP Models:
#Set up for conditional logistic regressions with SWReGAP 
#60
SW_Models[[1]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + strata(ID_Year), data = C60M)
#120
SW_Models[[2]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + strata(ID_Year), data = C120M)

#240
SW_Models[[3]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + vpd: 
#60
SW_Models[[4]]<- clogit(Burned ~ SWReGAP_LandcoverType2  * vpd_dsf + strata(ID_Year), data = C60M)

#120
SW_Models[[5]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf + strata(ID_Year), data = C120M)

#240
SW_Models[[6]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf  + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + def: 
#60
SW_Models[[7]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * def_dsf + strata(ID_Year), data = C60M)

#120
SW_Models[[8]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * def_dsf  + strata(ID_Year), data = C120M)

#240
SW_Models[[9]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * def_dsf  + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Tmax: 
SW_Models[[10]]<- clogit(Burned ~ SWReGAP_LandcoverType2  * Tmax_dsf  + strata(ID_Year), data = C60M)

#120
SW_Models[[11]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Tmax_dsf  + strata(ID_Year), data = C120M)

#240
SW_Models[[12]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Tmax_dsf  + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables  + PrefireNDVI: 
#60
SW_Models[[13]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + PFNDVI_dsf + strata(ID_Year), data = C60M)

#120
SW_Models[[14]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + PFNDVI_dsf + strata(ID_Year), data = C120M)

#240
SW_Models[[15]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + PFNDVI_dsf + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + PrefireNDVI + Ruggedness: 
#60
SW_Models[[16]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + strata(ID_Year), data = C60M)

#120
SW_Models[[17]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + strata(ID_Year), data = C120M)

#240
SW_Models[[18]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + PFNDVI_dsf + Ruggedness + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + PrefireNDVI + Ruggedness + Slope: 
#60
SW_Models[[19]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C60M)

#120
SW_Models[[20]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C120M)

#240
SW_Models[[21]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Aridity + PrefireNDVI + Ruggedness + Slope: 
#60
SW_Models[[22]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C60M)

#120
SW_Models[[23]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Aridity +  PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C120M)

#240
SW_Models[[24]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover * Aridity: 
#60
SW_Models[[25]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Aridity  + strata(ID_Year), data = C60M)

#120
SW_Models[[26]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Aridity + strata(ID_Year), data = C120M)

#240
SW_Models[[27]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Aridity + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + PrefireNDVI: 
#60
SW_Models[[28]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + PFNDVI_dsf + strata(ID_Year), data = C60M)

#120
SW_Models[[29]]<- clogit(Burned ~ SWReGAP_LandcoverType2 +  PFNDVI_dsf + strata(ID_Year), data = C120M)

#240
SW_Models[[30]]<- clogit(Burned ~ SWReGAP_LandcoverType2 + PFNDVI_dsf  + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover * Ruggedness: 
#60
SW_Models[[31]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Ruggedness + strata(ID_Year), data = C60M)

#120
SW_Models[[32]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Ruggedness + strata(ID_Year), data = C120M)

#240
SW_Models[[33]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Ruggedness + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover * Slope: 
#60
SW_Models[[34]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Slope + strata(ID_Year), data = C60M)

#120
SW_Models[[35]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Slope + strata(ID_Year), data = C120M)

#240
SW_Models[[36]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * Slope + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover * Fire Season Climate Variables: 
#60
SW_Models[[37]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + strata(ID_Year), data = C60M)

#120
SW_Models[[38]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + strata(ID_Year), data = C120M)

#240
SW_Models[[39]]<- clogit(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + strata(ID_Year), data = C240M)

summary(SW_Models[[24]])


#Begin Model selection Process: 

SW_modelnames= c(1:39) #Model names 

SW_modeltable= aictab(cand.set = SW_Models, modnames = SW_modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
SW_modeltable

#For some reason the aictab function is giving me the error: You must use the same response variable for all models, even though they are using the same response variable.This error just began this week and I have yet been able to resolve it. For this reason below is a AIC table made manually.

M1<-AIC(SW_Models[[1]])
M2<-AIC(SW_Models[[2]])
M3<-AIC(SW_Models[[3]])
M4<-AIC(SW_Models[[4]])
M5<-AIC(SW_Models[[5]])
M6<-AIC(SW_Models[[6]])
M7<-AIC(SW_Models[[7]])
M8<-AIC(SW_Models[[8]])
M9<-AIC(SW_Models[[9]])
M10<-AIC(SW_Models[[10]])
M11<-AIC(SW_Models[[11]])
M12<-AIC(SW_Models[[12]])
M13<-AIC(SW_Models[[13]])
M14<-AIC(SW_Models[[14]])
M15<-AIC(SW_Models[[15]])
M16<-AIC(SW_Models[[16]])
M17<-AIC(SW_Models[[17]])
M18<-AIC(SW_Models[[18]])
M19<-AIC(SW_Models[[19]])
M20<-AIC(SW_Models[[20]])
M21<-AIC(SW_Models[[21]])
M22<-AIC(SW_Models[[22]])
M23<-AIC(SW_Models[[23]])
M24<-AIC(SW_Models[[24]])
M25<-AIC(SW_Models[[25]])
M26<-AIC(SW_Models[[26]])
M27<-AIC(SW_Models[[27]])
M28<-AIC(SW_Models[[28]])
M29<-AIC(SW_Models[[29]])
M30<-AIC(SW_Models[[30]])
M31<-AIC(SW_Models[[31]])
M32<-AIC(SW_Models[[32]])
M33<-AIC(SW_Models[[33]])
M34<-AIC(SW_Models[[34]])
M35<-AIC(SW_Models[[35]])
M36<-AIC(SW_Models[[36]])
M37<-AIC(SW_Models[[37]])
M38<-AIC(SW_Models[[38]])
M39<-AIC(SW_Models[[39]])

Model_List<-list(Model_Number= seq(1:39), AIC = c(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16,M17,M18,M19,M20,M21,M22,M23,M24,M25,M26,M27,M28,M29,M30,M31,M32,M33,M34,M35,M36,M37,M38,M39))

SW_Tab<- data.frame(Model_List)

################################################################################LANDFIRE Models: 

LF_Models=list() #create a empty list of models.

#Set up for conditional logistic regressions with LANDFIRE
#60
LF_Models[[1]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C60M)
#120
LF_Models[[2]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C120M)

#240
LF_Models[[3]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + vpd: 
#60
LF_Models[[4]]<- clogit(Burned ~ LANDFIRE_LandcoverType2  * vpd_dsf + strata(ID_Year), data = C60M)

#120
LF_Models[[5]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf + strata(ID_Year), data = C120M)

#240
LF_Models[[6]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf  + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + def: 
#60
LF_Models[[7]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * def_dsf + strata(ID_Year), data = C60M)

#120
LF_Models[[8]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * def_dsf  + strata(ID_Year), data = C120M)

#240
LF_Models[[9]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * def_dsf  + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Tmax: 
LF_Models[[10]]<- clogit(Burned ~ LANDFIRE_LandcoverType2  * Tmax_dsf  + strata(ID_Year), data = C60M)

#120
LF_Models[[11]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Tmax_dsf  + strata(ID_Year), data = C120M)

#240
LF_Models[[12]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Tmax_dsf  + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables  + PrefireNDVI: 
#60
LF_Models[[13]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + PFNDVI_dsf + strata(ID_Year), data = C60M)

#120
LF_Models[[14]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + PFNDVI_dsf + strata(ID_Year), data = C120M)

#240
LF_Models[[15]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + PFNDVI_dsf + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + PrefireNDVI + Ruggedness: 
#60
LF_Models[[16]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + strata(ID_Year), data = C60M)

#120
LF_Models[[17]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + strata(ID_Year), data = C120M)

#240
LF_Models[[18]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + PFNDVI_dsf + Ruggedness + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Fire Season Climate Variables + PrefireNDVI + Ruggedness + Slope: 
#60
LF_Models[[19]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C60M)

#120
LF_Models[[20]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C120M)

#240
LF_Models[[21]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + Aridity + PrefireNDVI + Ruggedness + Slope: 
#60
LF_Models[[22]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C60M)

#120
LF_Models[[23]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity +  PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C120M)

#240
LF_Models[[24]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + PFNDVI_dsf + Ruggedness + Slope + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover * Aridity: 
#60
LF_Models[[25]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity  + strata(ID_Year), data = C60M)

#120
LF_Models[[26]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + strata(ID_Year), data = C120M)

#240
LF_Models[[27]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Aridity + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover + PrefireNDVI: 
#60
LF_Models[[28]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + PFNDVI_dsf + strata(ID_Year), data = C60M)

#120
LF_Models[[29]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 +  PFNDVI_dsf + strata(ID_Year), data = C120M)

#240
LF_Models[[30]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 + PFNDVI_dsf  + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover * Ruggedness: 
#60
LF_Models[[31]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Ruggedness + strata(ID_Year), data = C60M)

#120
LF_Models[[32]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Ruggedness + strata(ID_Year), data = C120M)

#240
LF_Models[[33]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Ruggedness + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover * Slope: 
#60
LF_Models[[34]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Slope + strata(ID_Year), data = C60M)

#120
LF_Models[[35]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Slope + strata(ID_Year), data = C120M)

#240
LF_Models[[36]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * Slope + strata(ID_Year), data = C240M)

#Set up for conditional logistic regressions with Landcover * Fire Season Climate Variables: 
#60
LF_Models[[37]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + strata(ID_Year), data = C60M)

#120
LF_Models[[38]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + strata(ID_Year), data = C120M)

#240
LF_Models[[39]]<- clogit(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + strata(ID_Year), data = C240M)

summary(LF_Models[[24]])

#Begin Model selection Process: 

LF_modelnames= c(1:39) #Model names 

LF_modeltable<- aictab(cand.set= LF_Models, modnames = LF_modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table

LF_modeltable

#For some reason the aictab function is giving me the error: You must use the same response variable for all models, even though they are using the same response variable.This error just began this week and I have yet been able to resolve it. For this reason below is a AIC table made manually.
LF1<-AIC(LF_Models[[1]])
LF2<-AIC(LF_Models[[2]])
LF3<-AIC(LF_Models[[3]])
LF4<-AIC(LF_Models[[4]])
LF5<-AIC(LF_Models[[5]])
LF6<-AIC(LF_Models[[6]])
LF7<-AIC(LF_Models[[7]])
LF8<-AIC(LF_Models[[8]])
LF9<-AIC(LF_Models[[9]])
LF10<-AIC(LF_Models[[10]])
LF11<-AIC(LF_Models[[11]])
LF12<-AIC(LF_Models[[12]])
LF13<-AIC(LF_Models[[13]])
LF14<-AIC(LF_Models[[14]])
LF15<-AIC(LF_Models[[15]])
LF16<-AIC(LF_Models[[16]])
LF17<-AIC(LF_Models[[17]])
LF18<-AIC(LF_Models[[18]])
LF19<-AIC(LF_Models[[19]])
LF20<-AIC(LF_Models[[20]])
LF21<-AIC(LF_Models[[21]])
LF22<-AIC(LF_Models[[22]])
LF23<-AIC(LF_Models[[23]])
LF24<-AIC(LF_Models[[24]])
LF25<-AIC(LF_Models[[25]])
LF26<-AIC(LF_Models[[26]])
LF27<-AIC(LF_Models[[27]])
LF28<-AIC(LF_Models[[28]])
LF29<-AIC(LF_Models[[29]])
LF30<-AIC(LF_Models[[30]])
LF31<-AIC(LF_Models[[31]])
LF32<-AIC(LF_Models[[32]])
LF33<-AIC(LF_Models[[33]])
LF34<-AIC(LF_Models[[34]])
LF35<-AIC(LF_Models[[35]])
LF36<-AIC(LF_Models[[36]])
LF37<-AIC(LF_Models[[37]])
LF38<-AIC(LF_Models[[38]])
LF39<-AIC(LF_Models[[39]])

LFList<-list(Model_Number= seq(1:39), AIC = c(LF1,LF2,LF3,LF4,LF5,LF6,LF7,LF8,LF9,LF10,LF11,LF12,LF13,LF14,LF15,LF16,LF17,LF18,LF19,LF20,LF21,LF22,LF23,LF24,LF25,LF26,LF27,LF28,LF29,LF30,LF31,LF32,LF33,LF34,LF35,LF36,LF37,LF38,LF39))

LF_Tab<- data.frame(LFList)

################################################################################
                         #Start Random Points Analysis
################################################################################
setwd("C:/Users/matth/Desktop/Method_1_Analysis/PA_V5/Random_Points")
RP<-read.csv("Cleaned_Random_Points.csv")

#Create RP SWReGAP Model List
RP_SWReGAP_Models=list() #create a empty list of models.

#General linear model for random points:
RP_SWReGAP_Models[[1]] <- glm(Burned ~ SWReGAP_LandcoverType2, family = "binomial", data = RP)
summary(RP_SWReGAP_Models[[1]])

#GLM with all LC * vpd_dsf 
RP_SWReGAP_Models[[2]] <- glm(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf, family = "binomial", data = RP)
summary(RP_SWReGAP_Models[[2]])

#GLM with all LC * def_dsf 
RP_SWReGAP_Models[[3]] <- glm(Burned ~ SWReGAP_LandcoverType2 * def_dsf , family = "binomial", data = RP)
summary(RP_SWReGAP_Models[[3]])

#GLM with all LC * Tmax_dsf
RP_SWReGAP_Models[[4]] <- glm(Burned ~ SWReGAP_LandcoverType2 *  Tmax_dsf, family = "binomial", data = RP)
summary(RP_SWReGAP_Models[[4]])

#GLM with all LC * vpd_dsf * def_dsf * Tmax_dsf
RP_SWReGAP_Models[[5]] <- glm(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf, family = "binomial", data = RP)
summary(RP_SWReGAP_Models[[5]])

#GLM with all LC * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf
RP_SWReGAP_Models[[6]] <- glm(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + BWI_10yrs, family = "binomial", data = RP)
summary(RP_SWReGAP_Models[[6]])

#GLM with all LC * vpd_dsf * def_dsf * Tmax_dsf  + PFNDVI_dsf + Ruggedness
RP_SWReGAP_Models[[7]] <- glm(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + BWI_10yrs + Ruggedness, family = "binomial", data = RP)
summary(RP_SWReGAP_Models[[7]])

#GLM with all LC * vpd_dsf * def_dsf * Tmax_dsf + PFNDVI_dsf + Ruggedness + Slope
RP_SWReGAP_Models[[8]] <- glm(Burned ~ SWReGAP_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf  + BWI_10yrs + Ruggedness + Slope, family = "binomial", data = RP)
summary(RP_SWReGAP_Models[[8]])

#GLM with all LC * Aridity + PFNDVI_dsf + Ruggedness + Slope
RP_SWReGAP_Models[[9]] <- glm(Burned ~ SWReGAP_LandcoverType2 * Aridity  + BWI_10yrs + Ruggedness + Slope, family = "binomial", data = RP)
summary(RP_SWReGAP_Models[[9]])

#Begin Model selection Process: 

RP_SWReGAP_modelnames= c() #Model names 

RP_SWReGAP_modeltable= aictab(cand.set = RP_SWReGAP_Models, modnames = RP_SWReGAP_modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
RP_SWReGAP_modeltable


#Create RP Landfire Model List
RP_LF_Models=list() #create a empty list of models.

#GLM with all LC 
RP_LF_Models[[1]] <- glm(Burned ~ LANDFIRE_LandcoverType2, family = "binomial", data = RP)
summary(RP_LF_Models[[1]])

#GLM with all LC * vpd_dsf 
RP_LF_Models[[2]] <- glm(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf, family = "binomial", data = RP)
summary(RP_LF_Models[[2]])

#GLM with all LC * def_dsf 
RP_LF_Models[[3]] <- glm(Burned ~ LANDFIRE_LandcoverType2 * def_dsf, family = "binomial", data = RP)
summary(RP_LF_Models[[3]])

#GLM with all LC * Tmax_dsf
RP_LF_Models[[4]] <- glm(Burned ~ LANDFIRE_LandcoverType2 * Tmax_dsf, family = "binomial", data = RP)
summary(RP_LF_Models[[4]])

#GLM with all LC * vpd_dsf * def_dsf * Tmax_dsf
RP_LF_Models[[5]] <- glm(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf, family = "binomial", data = RP)
summary(RP_LF_Models[[5]])

#GLM with all LC * vpd_dsf * def_dsf * Tmax_dsf + BWI_10yrs + PFNDVI_dsf
RP_LF_Models[[6]] <- glm(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + BWI_10yrs, family = "binomial", data = RP)
summary(RP_LF_Models[[6]])

#GLM with all LC * vpd_dsf * def_dsf * Tmax_dsf + BWI_10yrs + PFNDVI_dsf + Ruggedness
RP_LF_Models[[7]] <- glm(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + BWI_10yrs  + Ruggedness, family = "binomial", data = RP)
summary(RP_LF_Models[[7]])

#GLM with all LC * vpd_dsf * def_dsf * Tmax_dsf + BWI_10yrs + PFNDVI_dsf + Ruggedness + Slope
RP_LF_Models[[8]] <- glm(Burned ~ LANDFIRE_LandcoverType2 * vpd_dsf * def_dsf * Tmax_dsf + BWI_10yrs  + Ruggedness + Slope, family = "binomial", data = RP)
summary(RP_LF_Models[[8]])

#GLM with all LC * Aridity + BWI_10yrs + PFNDVI_dsf + Ruggedness + Slope
RP_LF_Models[[9]] <- glm(Burned ~ LANDFIRE_LandcoverType2 * Aridity + BWI_10yrs  + Ruggedness + Slope, family = "binomial", data = RP)
summary(RP_LF_Models[[9]])

RP_LF_modelnames= c() #Model names 

RP_LF_modeltable= aictab(cand.set = RP_LF_Models, modnames = RP_LF_modelnames, second.ord = TRUE, nobs=NULL, sort=TRUE) #Make model selection table
RP_LF_modeltable 

#Begin subseting based on years since fire and DOB Area in order to run clogits as normal on subseted dataframes: 

