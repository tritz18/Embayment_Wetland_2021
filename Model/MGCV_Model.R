library(tidyverse)
library(lubridate)
library(stats)
library(broom)
library(mgcv)
library(readxl)
library(gratia)

##################
setwd("/Users/thorn/OneDrive/Desktop/Embayment_Wetland_2021/Embayment_Wetland_R_Project/Model")

Embayment<- read_xlsx("Embayment_Model.xlsx")
Embayment$Date_Time<-as.POSIXct(Embayment$Date_Time, format="%Y-%m-%d")
Embayment$Species<-as.factor(Embayment$Species)
Embayment$Location<-as.factor(Embayment$Location)
Embayment$Catch<-as.numeric(Embayment$Catch)

ggplot(Embayment, aes(Study_Day, TEMP_max))+
  geom_point()+geom_smooth()+
  facet_wrap(~Species, scales="free_y")

Embayment<-scale(Mean_Length, center = TRUE, scale = TRUE)

A <- gam(Mean_Length~ s(DO_Change, by=Location)+s(TEMP_max, by=Location)+
           Study_Day+Species,
         data = Embayment, method = "REML")
summary(A)
draw(A)
appraise(A)
derivatives(A)
confint.gam(A)

gam.check(A)
