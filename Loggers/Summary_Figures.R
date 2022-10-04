#### Packages ####
library(tidyverse)
library(lubridate)
library(stats)
library(broom)
library(ggpubr)
library(ggh4x)
##################

################################################################################################################################
#############################           Abiotic Data       Summary & Visulizations                                  ################################
###############################################################################################################################

#### Set working directory, where logger files are ###
setwd("/Users/thorn/OneDrive/Desktop/Embayment_Wetland_2021/Embayment_Wetland_R_Project/Loggers/")
f = list.files(pattern="*.csv")

#### Clean raw logger data and summarize (48 measurements per day, DO & TEMP) #### 

Emb_Wetlands_Raw <- purrr::map_df(f, function(x) {
  mydata <- read.csv(x, header = TRUE)
  mydata$Date_Time <- as.POSIXct(mydata$Date_Time,  format="%Y-%m-%d")
  mydata %>%
    filter(Date_Time < "2021-07-23" & Date_Time > "2021-06-11") %>% 
    select(DO, TEMP, Date_Time, Location, Site) %>%
    group_by(Date_Time, Location)
})


##########################################

#### Abiotic hours data creation (1 measurement per day, sites grouped by location) #### 
Abiotic_Hrs<- Emb_Wetlands_Raw %>% 
  group_by(across(c(Location, Date_Time, Site))) %>% 
  select(DO, TEMP) %>%
  summarise(Hyp_Hrs=sum(DO<=3.99)/2, Nor_Hrs=sum(DO>=4.00)/2,
            Temp_Hrs=sum(TEMP>=28)/2) 

#### Summary of abiotic conditions (48 measurements --> 1 per day measurements (mean DO , max temp),sites grouped by location ) ####
Abiotic_Sum<- Emb_Wetlands_Raw %>% 
  group_by(across(c(Location, Date_Time, Site))) %>% 
  select(DO,TEMP) %>% 
  summarise(across(c(DO,TEMP), list(mean = mean, max = max, min=min, sd=sd), .names = "{col}_{fn}"))

#### Combine Datafiles for Abiotic Final (1 per day from summarized loggers and abiotic hours file #### 
Abiotic_Final<- left_join(Abiotic_Hrs,Abiotic_Sum, by=c("Date_Time", "Location", "Site")) %>% 
  group_by(Date_Time) %>% 
  mutate(Study_Day = cur_group_id()) ## Add study day instead of date ##


##############################################################################
####                  Visulizations                                   ####
#############################################################################


#### Box and whisker from raw data (48 measurements per day, grouped by site)####

EW_Gather<- Emb_Wetlands_Raw %>% 
  gather(key = "Abiotic", value = "Measurement", -Date_Time, -Location, -Site) %>% 
  mutate(Abiotic=factor(Abiotic)) 

#### set unique scales for facetting ####
scales_EWG <- list(
  scale_y_continuous(breaks = seq(0,18,2)), 
  scale_y_continuous(breaks = seq(14,34,2)))


#### Plot with faceting edits to show both y axis labels and unique scales ####
ggplot(EW_Gather, aes(Location, Measurement, group=interaction(Location, Site), color=Site))+
  stat_boxplot(geom ='errorbar')+geom_boxplot(fill="#CCCCCC")+
  scale_color_manual(values = c("#0072B2","#009E73"))+theme_bw()+
  theme(legend.position = "bottom", axis.title = element_blank(),
        strip.text = element_text(size = 12), axis.text = element_text(size=11),
        strip.background = element_blank(), strip.placement = "outside")+
  facet_wrap(~Abiotic, scales="free_y",labeller = as_labeller(
    c(DO = "Dissolved Oxygen (mg/l)", TEMP = "Temperature (C°)")), 
    strip.position = "left")+
  facetted_pos_scales(y = scales_EWG)

ggsave("Embayment_Wetland_Box_Whisker.png", width = 5, height = 4, dpi=300)

##############################################################################
####                  Abiotic Visualization                          ####
#############################################################################

EWH_Gather<- Abiotic_Hrs %>% 
  gather(key = "Abiotic", value = "Hours", -Date_Time, -Location, -Site) %>% 
  mutate(Abiotic=factor(Abiotic)) 


#### Plot with faceting edits to show both y axis labels and unique scales ####
ggplot(EWH_Gather, aes(Location, Hours, group=interaction(Location, Site), color=Site))+
  stat_boxplot(geom ='errorbar')+geom_boxplot(fill="#CCCCCC")+
  scale_color_manual(values = c("#0072B2","#009E73"))+scale_y_continuous(breaks = seq(0,12,1))+
  theme_bw()+
  theme(legend.position = "bottom", axis.title.x = element_blank(),
        strip.text = element_text(size = 12), axis.text = element_text(size=11))+
  facet_wrap(~Abiotic,labeller = as_labeller(
    c(Hyp_Hrs = "Hypoxia (DO<3.99 mg/l)", Nor_Hrs = "Normoxia (DO>4.00 mg/l)", Temp_Hrs="Water Temperature > 27°C")))
ggsave("Embayment_Wetland_Hours_Box_Whisker.png", width = 8, height = 5, dpi=300)


