#Perimeter Analysis Figures 
#Matt Harris (8/6/2023)

#Load Packages: 
library(survival)
library(MuMIn)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(AICcmodavg)

#Produce proportion burned for random points:
setwd("C:/Users/matth/Desktop/Method_1_Analysis/PA_V5/Random_Points")
RP<-read.csv("Cleaned_Random_Points.csv")

#Organize: 
RP<- RP %>%mutate(SWReGAP_LandcoverType = replace(SWReGAP_LandcoverType, which(SWReGAP_LandcoverType == ""), NA))  %>% as.data.frame()
RP<- RP %>%mutate(SWReGAP_LandcoverType2 = replace(SWReGAP_LandcoverType2, which(SWReGAP_LandcoverType2 == ""), NA))  %>% as.data.frame()
RP<- RP %>%mutate(LANDFIRE_LandcoverType = replace(LANDFIRE_LandcoverType, which(LANDFIRE_LandcoverType == ""), NA))  %>% as.data.frame()
RP<- RP %>%mutate(LANDFIRE_LandcoverType2 = replace(LANDFIRE_LandcoverType2, which(LANDFIRE_LandcoverType2 == ""), NA))  %>% as.data.frame()

#Proportion Burned by Landcover (SW)
RP1 <- RP %>%
  group_by(SWReGAP_LandcoverType2) %>%
  summarise(n = n(), unburned = sum(Burned == 0), burned = sum(Burned == 1)) %>%
  gather("key", "value", - c(SWReGAP_LandcoverType2, n)) %>%
  ggplot(aes( x = SWReGAP_LandcoverType2, y = value, group = key, fill = key)) +
  geom_col(position = "dodge") +
  labs(x = "Land Cover Type", y = "Proportion Burned", title = "Proportion Burned By Land Cover Type (Random Points:SWReGAP)")

RP1

#Area Burned by Landcover (SW)
RP2 <- RP %>%
  group_by(SWReGAP_LandcoverType2) %>%
  summarise(n = n(), Area_burned = sum(DOB_Area, na.rm = T)/10000000000) %>%
  gather("key", "value", - c(SWReGAP_LandcoverType2, n)) %>%
  ggplot(aes( x = SWReGAP_LandcoverType2, y = value, group = key, fill = key)) +
  geom_col(position = "dodge") +
  labs(x = "Land Cover Type", y = "Proportion Burned", title = "Proportions of Area Burned By Land Cover Type in Millions of ha (Random Points:SWReGAP)")
RP2

options(scipen = 100)

#Proportion Burned by Landcover (LF)
RP3 <- RP %>%
  group_by(LANDFIRE_LandcoverType2) %>%
  summarise(n = n(), unburned = sum(Burned == 0), burned = sum(Burned == 1)) %>%
  gather("key", "value", - c(LANDFIRE_LandcoverType2, n)) %>%
  ggplot(aes( x = LANDFIRE_LandcoverType2, y = value, group = key, fill = key)) +
  geom_col(position = "dodge") +
  labs(x = "Land Cover Type", y = "Proportion Burned", title = "Proportion Burned By Land Cover Type (Random Points:LANDFIRE)")

RP3

#Area Burned by Landcover (LF)
RP4 <- RP %>%
  group_by(LANDFIRE_LandcoverType2) %>%
  summarise(n = n(), Area_burned = sum(DOB_Area, na.rm = T)/10000000000) %>%
  gather("key", "value", - c(LANDFIRE_LandcoverType2, n)) %>%
  ggplot(aes( x = LANDFIRE_LandcoverType2, y = value, group = key, fill = key)) +
  geom_col(position = "dodge") +
  labs(x = "Land Cover Type", y = "Proportion Burned", title = "Proportions of Area Burned By Land Cover Type in Millions of ha (Random Points:LANDFIRE)")
RP4

#Burn Occurrence By Ruggedness
figure_RP_Rug <- ggplot() +
  geom_smooth(data = RP, aes(x= Ruggedness, y = Burned, color=SWReGAP_LandcoverType2), method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  ylim(0,1)+
  labs(title = "Random Points: Burn Occurrence By Ruggedness")
figure_RP_Rug

#Burn Occurence by Elevation
figure_RP_Elv <- ggplot() +
  geom_smooth(data = RP, aes(x= Elevation, y = Burned), method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  coord_cartesian(ylim=c(0, 0.5))+
  labs(title= "GLM: Burn Occurence by Elevation")
figure_RP_Elv

#Burn Occurence by Slope
figure_RP_Slp <- ggplot() +
  geom_smooth(data = RP, aes(x= Slope, y = Burned), method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  coord_cartesian(ylim=c(0, 0.5))+
  labs(title= "GLM: Burn Occurence by Slope")
figure_RP_Slp

#Burn Occurence by Aridity
figure_RP_Ard <- ggplot() +
  geom_smooth(data = RP, aes(x= Aridity, y = Burned), method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  coord_cartesian(ylim=c(0, 0.5))+
  labs(title= "GLM: Burn Occurence by Aridity")
figure_RP_Ard

#Prop burned accounting for frequency (how often a landcover type occurs: 
PBF1<- SW_PB %>%
  group_by(SW_Type) %>%
  summarise(n = n(), Prop_Burned_BYLC) %>%
  gather("key", "value", - c(SW_Type, n)) %>%
  ggplot(aes( x = SW_Type, y = value, group = key, fill = key)) +
  geom_col(position = "dodge") +
  labs(x = "Land Cover Type", y = "Percent Burned", title = "Proportion Burned By Land Cover Type Accounting for LC Frequency (Random Points:SWReGAP)")
PBF1

PBF2<- LF_PB %>%
  group_by(LF_Type) %>%
  summarise(n = n(), Prop_Burned_BYLC) %>%
  gather("key", "value", - c(LF_Type, n)) %>%
  ggplot(aes( x = LF_Type, y = value, group = key, fill = key)) +
  geom_col(position = "dodge") +
  labs(x = "Land Cover Type", y = "Percent Burned", title = "Proportion Burned By Land Cover Type Accounting for LC Frequency (Random Points:LANDFIRE)")
PBF2

#Aridity by Landcover: 
ARD1<- RP %>%
  group_by(LANDFIRE_LandcoverType2) %>%
  summarise(n = n(), Aridity) %>%
  gather("key", "value", - c(LANDFIRE_LandcoverType2, n)) %>%
  ggplot(aes( x = LANDFIRE_LandcoverType2, y = value, group = key, fill = key)) +
  geom_col(position = "dodge") +
  labs(x = "Land Cover Type", y = "Percent Burned", title = "Aridity By Land Cover Type (Random Points:LANDFIRE)")
ARD1

ARD2<- RP %>%
  group_by(SWReGAP_LandcoverType2) %>%
  summarise(n = n(), Aridity) %>%
  gather("key", "value", - c(SWReGAP_LandcoverType2, n)) %>%
  ggplot(aes( x = SWReGAP_LandcoverType2, y = value, group = key, fill = key)) +
  geom_col(position = "dodge") +
  labs(x = "Land Cover Type", y = "Percent Burned", title = "Aridity By Land Cover Type (Random Points:SWReGAP)")
ARD2
