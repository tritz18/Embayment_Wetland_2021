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


#### Combine Datafiles #### 
Abiotic_Final<- left_join(Abiotic_Hrs,Abiotic_Sum, by=c("Date_Time", "Location")) %>% 
  group_by(Date_Time) %>% 
  mutate(Study_Day = cur_group_id())


