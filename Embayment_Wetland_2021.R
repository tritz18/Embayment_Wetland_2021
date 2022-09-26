#### Packages ####

library(lubridate)
library(stats)
library(tidyr)
library(fpp3)
library(broom)
library(fracdiff)
library(factoextra)
library(tidyverse)

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
###################                                   Catch Data                                                ###############
###############################################################################################################################

#### set working directory to catch data #####

setwd("/Users/thorn/OneDrive/Desktop/Embayment_Wetland_2021/Embayment_Wetland_R_Project/Catch_Data")

#### Format catch data ####
Catch_Raw<- read.csv("EM_2021_Catch.csv")
Catch_Raw$Date_Time<-as.POSIXct(Catch_Raw$Date_Time, format="%Y-%m-%d")
Catch_Raw$Catch<- as.numeric(Catch_Raw$Catch) 
Catch_Raw$Depth<- as.numeric(Catch_Raw$Depth) 

#### Create depth data set ####
Depth<- Catch_Raw %>% 
  group_by(across(c(Date_Time,Location))) %>% 
  select(Depth) %>%
  summarise(Mean_Depth=mean(Depth), Max_Depth=max(Depth), Min_Depth=min(Depth))


#### Create Catch dataset grouped by species, date and location ####
Catch_Sum <-Catch_Raw %>%
  filter(Species %in% c("LMB", "SMB", "LEPOMIS", "CYPRINIDAE", "KILLI", "BBH"), LIFE_STAGE %in% c("YOY")) %>% 
  group_by(across(c(Location, Species, Date_Time))) %>% 
  summarise(Catch= (sum(Catch)))


#### Join depth to catch data ####
Catch_Sum <- left_join(Catch_Sum,Depth, by=c("Date_Time", "Location"))



