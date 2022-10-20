#### Packages ####
library(tidyverse)
library(lubridate)
library(stats)
library(broom)
library(ggpubr)
library(ggh4x)
##################

###################################################################################################################
###################################################################################################################
#############################           Abiotic Data       Summary & Visualizations                  ##############           
###################################################################################################################
###################################################################################################################

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
  group_by(across(c(Location, Date_Time))) %>% 
  select(DO, TEMP) %>%
  summarise(Hyp_Hrs=sum(DO<=3.99)/2, Nor_Hrs=sum(DO>=4.00)/2,
            Temp_Hrs=sum(TEMP>=28)/2) 

#### Summary of abiotic conditions (48 measurements --> 1 per day measurements (mean DO , max temp),sites grouped by location ) ####
Abiotic_Sum<- Emb_Wetlands_Raw %>% 
  group_by(across(c(Location, Date_Time))) %>% 
  select(DO,TEMP) %>% 
  summarise(across(c(DO,TEMP), list(mean = mean, max = max, min=min, sd=sd), .names = "{col}_{fn}"))

#### Combine Datafiles for Abiotic Final (1 per day from summarized loggers and abiotic hours file #### 
Abiotic_Final<- left_join(Abiotic_Hrs,Abiotic_Sum, by=c("Date_Time", "Location")) %>% 
  group_by(Date_Time) %>% 
  mutate(Study_Day = cur_group_id()) ## Add study day instead of date ##


##############################################################################
####                  Visulizations                                     ####
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
####                  Abiotic - Hours Visualization                      ####
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



#########################################################################################################
#########################################################################################################
#############################           Fish Data                         ###############################
#########################################################################################################
#########################################################################################################


##################################################################################
####                  Fish Catch and Shannon's                                ####
##################################################################################

setwd("/Users/thorn/OneDrive/Desktop/Embayment_Wetland_2021/Embayment_Wetland_R_Project/Catch")

#### Format catch data ####
Catch_Raw<- read.csv("EM_2021_Catch.csv")
Catch_Raw$Date<-as.POSIXct(Catch_Raw$Date, format="%Y-%m-%d")
Catch_Raw$Catch<- as.numeric(Catch_Raw$Catch) 

#### Function for Shannon diversity index (cause vegan not tidy)
shannon<- function(x){
  
  rabund<- x[x>0]/sum(x)
  -sum(rabund *log(rabund))
}
#### Create Catch dataset grouped by species, date and location ####
Catch_Sum <-Catch_Raw %>%
  filter(Species %in% c("LMB", "SMB", "LEPOMIS", "CYPRINIDAE", "KILLI", "BBH", "CARP"), LIFE_STAGE %in% c("YOY")) %>% 
  group_by(across(c(Location, Species, Date))) %>% 
  summarise(Catch= ((sum(Catch)))) 

Shannon_Data<- Catch_Sum %>% 
  group_by(across(c(Date, Location))) %>%
  select(Catch) %>% 
  summarise(Shan_Div = shannon(Catch)) %>% 
  group_by(Date) %>% 
  mutate(Study_Day = cur_group_id())

Catch_Shannon<- left_join(Catch_Sum, Shannon_Data, by=c("Date", "Location")) %>% 
  group_by(Date) %>% 
  mutate(Study_Day = cur_group_id())

#################################################################################
#################         Visualizations of Catch Data         ##################
#################################################################################

ggplot(Catch_Shannon, aes(Study_Day, log10(Catch), color=Location,
  group=Location))+
  geom_col(fill="#CCCCCC", width=0.5)+
  theme_bw()+theme(legend.position = "bottom", strip.text = element_text(size=12), 
  axis.text = element_text(size=11), axis.title = element_text(size=11.5),
  legend.title = element_blank())+
scale_color_manual(values=c("#999999", "#000000"))+ylab("Log10 Catch")+xlab("Study Day")+
  scale_y_continuous(breaks=seq(0,5,1))+
  facet_wrap(~Species, scales="free_y")

ggplot(Shannon_Data, aes(Study_Day, Shan_Div, color=Location,
  group=Location))+
  geom_col(fill="#CCCCCC", width=0.5)+
  theme_bw()+theme(legend.position = "bottom")+scale_color_manual(values=c("#999999", "#000000"))


#################################################################################
#################       Visualizations of LW Data              ##################
#################################################################################

setwd("/Users/thorn/OneDrive/Desktop/Embayment_Wetland_2021/Embayment_Wetland_R_Project/Catch")


#### Data set up and cleaning ####
LW_21_Raw<- read.csv("LW_YOY_2021.csv")  
LW_21_Raw$Date<-as.POSIXct(LW_21_Raw$Date, format="%Y-%m-%d")
LW_21_Raw$Length<- as.numeric(LW_21_Raw$Length) 
LW_21_Raw$Weight<- as.numeric(LW_21_Raw$Weight)

#### Cleaning data and create nesting to run lm by species ####
LW_21<- LW_21_Raw %>% 
  filter(Species %in% c("LMB", "SMB", "LEPOMIS", "CYPRINIDAE", "KILLI", "CARP", "BBH")) %>% 
  select(-Notes)

#### Log transform length and weight ####
LW_21<- LW_21 %>% 
 mutate(Length=(log10(Length)), Weight=(log10(Weight)))

Condition_Metrics<-LW_21 %>% 
  group_by(across(c(Species, Date, Location))) %>% 
  mutate(K=Weight/(Length^3)*100,000) %>% 
  group_by(across(c(Species, Date, Location))) %>% 
  summarise(Mean_K=mean(K), Mean_Length=mean(Length), Mean_Weight=mean(Weight), SD_K=sd(K,na.rm = FALSE )) %>% 
  group_by(Date) %>% 
  mutate(Study_Day = cur_group_id()) %>% 
  na.omit()

#######

ggplot(Condition_Metrics, aes(Study_Day, Mean_Length, group=Location, color=Location))+
  geom_point()+geom_smooth(method="lm")+
  stat_regline_equation(aes(alpha=0.5, label = paste(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))),
                        formula = y~x)+
  theme_bw()+facet_wrap(~Species, scales = "free_y")

ggplot(LW_21, aes(Length, Weight, group=Location))+
  geom_point()+geom_smooth(method="lm")+
  stat_regline_equation(aes(alpha=0.5, label = paste(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))), 
                        formula = y~x)+
  theme_bw()+facet_wrap(~Species)

ggplot(Condition_Metrics, aes(Study_Day, Mean_Length, group=Location, color=Location))+
  geom_point()+geom_smooth(method="lm")+
  stat_regline_equation(aes(alpha=0.5, label = paste(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))),
                        formula = y~x)+
  theme_bw()+facet_wrap(~Location)















###########################################################################
####################      Catch - Abiotic #################################
###########################################################################
Catch_Abiotic<- left_join(Abiotic_Final,Catch_Shannon, by=c("Location", "Study_Day")) %>% 
  na.omit() %>% 
  select(-Date_Time, -Date)

ggplot(Catch_Abiotic)+
  geom_bar(aes(Study_Day, Shan_Div), stat = "identity")+
  geom_point(aes(Study_Day,Nor_Hrs*1))+geom_line(aes(Study_Day,Nor_Hrs*1))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~./1, name="Mean Daily DO (mg/l)"))+
  facet_wrap(~Species)


  
  
  
  
