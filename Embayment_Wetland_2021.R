#### Packages ####

library(lubridate)
library(stats)
library(tidyr)
library(fpp3)
library(broom)
library(fracdiff)
library(factoextra)
library(tidyverse)
library(ggpubr)
options(scipen=999)
##################

################################################################################################################################
#############################           Abiotic Data                                           ################################
###############################################################################################################################

#### Set working directory, where logger files are ###
setwd("/Users/thorn/OneDrive/Desktop/Embayment_Wetland_2021/Embayment_Wetland_R_Project/")
f = list.files(pattern="*.csv")

#### Clean data and summarize #### 

Emb_Wetlands_Raw <- purrr::map_df(f, function(x) {
  mydata <- read.csv(x, header = TRUE)
  mydata$Date_Time <- as.POSIXct(mydata$Date_Time,  format="%Y-%m-%d")
  mydata %>%
    filter(Date_Time < "2021-07-23" & Date_Time > "2021-06-11") %>% 
    select(DO, TEMP, Date_Time, Location) %>%
    group_by(Date_Time, Location)
})

##########################################

#### Abiotic hours data creation #### 
Abiotic_Hrs<- Emb_Wetlands_Raw %>% 
  group_by(across(c(Location, Date_Time))) %>% 
  select(DO, TEMP) %>%
  summarise(Hyp_Hrs=sum(DO<3.01)/2, Nor_Hrs=sum(DO>3.00)/2,
            Temp_Hrs=sum(TEMP>28)/2) 


#### Summary of abiotic conditions ####
Abiotic_Sum<- Emb_Wetlands_Raw %>% 
  group_by(across(c(Location, Date_Time))) %>% 
  select(DO,TEMP) %>% 
  summarise(across(c(DO,TEMP), list(mean = mean, max = max, min=min, sd=sd), .names = "{col}_{fn}"))


#### Combine Datafiles for Abiotic Final#### 
Abiotic_Final<- left_join(Abiotic_Hrs,Abiotic_Sum, by=c("Date_Time", "Location")) %>% 
  group_by(Date_Time) %>% 
  mutate(Study_Day = cur_group_id())

###############################################################################################################################
###################                                   Fish Data                                                ###############
###############################################################################################################################

#### set working directory to catch data #####

setwd("/Users/thorn/OneDrive/Desktop/Embayment_Wetland_2021/Embayment_Wetland_R_Project/Catch_Data")

#### Format catch data ####
Catch_Raw<- read.csv("EM_2021_Catch.csv")
Catch_Raw$Date<-as.POSIXct(Catch_Raw$Date, format="%Y-%m-%d")
Catch_Raw$Catch<- as.numeric(Catch_Raw$Catch) 
Catch_Raw$Depth<- as.numeric(Catch_Raw$Depth) 

#### Create depth data set ####
Depth<- Catch_Raw %>% 
  group_by(across(c(Date,Location))) %>% 
  select(Depth) %>%
  summarise(Mean_Depth=mean(Depth), Max_Depth=max(Depth), Min_Depth=min(Depth))


#### Create Catch dataset grouped by species, date and location ####
Catch_Sum <-Catch_Raw %>%
  filter(Species %in% c("LMB", "SMB", "LEPOMIS", "CYPRINIDAE", "KILLI", "BBH"), LIFE_STAGE %in% c("YOY")) %>% 
  group_by(across(c(Location, Species, Date))) %>% 
  summarise(Catch= (sum(Catch)))


#### Join depth to catch data ####
Catch_Sum <- left_join(Catch_Sum,Depth, by=c("Date", "Location"))

#### LW metrics #### 

#### Data set up ####
LW_21_Raw<- read.csv("LW_YOY_2021.csv")  
LW_21_Raw$Date<-as.POSIXct(LW_21_Raw$Date, format="%Y-%m-%d")
LW_21_Raw$Length<- as.numeric(LW_21_Raw$Length) 
LW_21_Raw$Weight<- as.numeric(LW_21_Raw$Weight)


#### Cleaning data and create nesting to run lm by species ####
LW_21<- LW_21_Raw %>% 
  filter(Species %in% c("LMB", "SMB", "LEPOMIS", "CYPRINIDAE", "KILLI", "CARP")) %>% 
  select(-Notes)

split_by_species<- 
  LW_21 %>% 
  group_nest(Species, Date, Location)

#### Linear model by to pull residuals by species ####


LM_Species<- 
  split_by_species %>% 
  mutate(model=map(data, ~lm(Length ~ Weight, data = .x)))

Species_Residuals<- LM_Species %>% 
  mutate(residuals=map(model, residuals)) %>% 
  select(Species,Date, Location, residuals) %>% 
  unnest(cols=c(residuals)) 

Species_Residuals$residuals<- as.integer(Species_Residuals$residuals, length=3)

#### Summarize residuals ####

Species_Residuals<- Species_Residuals %>% 
  group_by(across(c(Date, Species))) %>% 
  select(residuals) %>% 
  summarise(Mean_LWR=(mean(residuals)), SD_LWR=(sd(residuals))) %>% 
  na.omit() 

#### Summarize condition metrics ####
Condition_Metrics<-LW_21 %>% 
  group_by(across(c(Species, Date, Location))) %>% 
  mutate(K=Weight/(Length^3)*100,000) %>% 
  group_by(across(c(Species, Date, Location))) %>% 
  summarise(Mean_K=mean(K), Mean_Length=mean(Length), Mean_Weight=mean(Weight), SD_K=sd(K,na.rm = FALSE )) %>% 
  na.omit()

#### Joining data files #### 

Fish_Metric_Final<-  right_join(Species_Residuals, Condition_Metrics, by=c("Species", "Date")) %>% 
  na.omit() %>% 
  group_by(Date) %>% 
  mutate(Study_Day = cur_group_id()) 

YOY_Final<- left_join(Fish_Metric_Final, Catch_Sum, by = c("Date", "Location", "Species"))


ggplot(YOY_Final, aes(Study_Day, Mean_Length))+
  geom_point()+geom_smooth(method="lm")+
  stat_regline_equation(aes(alpha=0.5, label = paste("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
                        label.x = 0, label.y =50, formula = y~x)+
  theme_bw()+facet_wrap(~Species, scales="free_y")

