#### Packages ####
library(tidyverse)
library(lubridate)
library(stats)
library(broom)
library(ggpubr)
library(readxl)
##################

################################################################################################################################
#############################           Abiotic Data                                           ################################
###############################################################################################################################

#### Set working directory, where logger files are ###
setwd("/Users/thorn/OneDrive/Desktop/Embayment_Wetland_2021/Embayment_Wetland_R_Project/Loggers/Figures/")
f = list.files(pattern="*.xlsx")

#### Clean raw logger data and summarize (48 measurements per day - DO & TEMP) #### 


Emb_Wetlands_Raw <- purrr::map_df(f, function(x) {
  mydata <- read_excel(x)
  mydata$Date_Time <- as.POSIXct(mydata$Date_Time,  format="%Y/%m/%d %H:%M")
  mydata %>%
    select(DO, TEMP, Date_Time, Location, Site) %>% 
    group_by(Location, Site, Date_Time)
})

Emb_Wetlands_Raw$Site<- factor(Emb_Wetlands_Raw$Site, levels=c("Littoral", "Exterior", "Interior"))
Emb_Wetlands_Raw$Location<- as.factor(Emb_Wetlands_Raw$Location)

##########################################


Abiotic_Daily_Summary<- Emb_Wetlands_Raw %>% 
  mutate(Date = format(as.POSIXct(Date_Time), format= "%y/%m/%d")) %>% 
  group_by(across(c(Location, Date, Site))) %>% 
  select(DO,TEMP) %>% 
  summarise(across(c(DO,TEMP), list(mean = mean, min=min, max=max, sd=sd), .names = "{col}_{fn}"))

Abiotic_Daily_Summary$Date<- as.POSIXct(Abiotic_Daily_Summary$Date, format="%y/%m/%d")
Abiotic_Daily_Summary$Site<- factor(Abiotic_Daily_Summary$Site, levels=c("Littoral", "Exterior", "Interior"))


ggplot(Abiotic_Daily_Summary, aes(Date, DO_mean, group=interaction(Location, Site)))+
  geom_line()+theme_bw()+theme(axis.text.x =element_text(angle=90))+
  scale_x_datetime(breaks = ("30 days"))+
  facet_wrap(~Location+Site)

ggplot(Abiotic_Daily_Summary, aes(Date, DO_mean, group=interaction(Location,Site), color=Site))+
  geom_line(size=0.8)+geom_hline(yintercept=3, color="#D55E00", lty=2)+geom_hline(yintercept = 0, color="#D55E00")+
  theme_bw()+theme(axis.text.x =element_text(angle=90), legend.position = "bottom", legend.title = element_blank(),
                   strip.text = element_text(size=13),
                   axis.text = element_text(size=13),
                   axis.title.x = element_blank(),
                   axis.title.y = element_text(size=13),
                   legend.text = element_text(size = 13))+
  scale_color_manual(values = c("#0072B2", "#009E73", "#999999"))+
  scale_x_datetime(breaks= seq(min(Abiotic_Daily_Summary$Date), max(Abiotic_Daily_Summary$Date), length=7),
                   date_labels = "%m/%d")+scale_y_continuous(breaks=seq(0,18,1))+
  ylab("Mean Daily Dissolved Oxygen (mg/l)")+
  facet_wrap(~Location)
ggsave("Mean_DO_Annual_Trend.png", dpi=300, width=7, height=5)


