####################

library(tidyverse)
library(lubridate)
library(stats)
library(broom)
library(ggpubr)
library(tidymodels)
library(future)
library(glmnet)
library(mgcv)
library(tidymv)
library(corrr)

tidymodels_prefer()

##################
setwd("/Users/thorn/OneDrive/Desktop/Embayment_Wetland_2021/Embayment_Wetland_R_Project/Model")

Embayment<- read.csv("Embayment_Wetland_Model.csv")
Embayment$Date_Time<-as.POSIXct(Embayment$Date_Time, format="%Y-%m-%d")
Embayment$Species<-as.factor(Embayment$Species)
Embayment$Location<-as.factor(Embayment$Location)
Embayment$Catch<-as.numeric(Embayment$Catch)


Embayment_Cor<- Embayment %>% 
  select(-Location, -Date_Time) %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(
    correlations=map(data,correlate)) %>% 
      unnest(correlations) %>% 
  filter(term %in% c("Catch")) %>% 
  ungroup() %>% 
  select(-data, -Species) %>% 
  summarise(across(everything(), mean))


###########################

ggplot(Embayment, aes(Catch))+
  geom_histogram()

ggplot(Embayment, aes(DO_Change, Mean_Length))+
  geom_point()+geom_line()+
  facet_wrap(~Species, scales="free")

ggplot(Embayment, aes(Study_Day,Catch))+
  geom_point()+geom_smooth(method="lm")+
  stat_regline_equation(aes(alpha=0.5, label = paste("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
                        label.x = -1, label.y =1.6, formula = y~x)+
  theme_bw()+facet_wrap(~Species, scales = "free_y")

ggplot(Embayment, aes(Catch,Mean_Length))+
  geom_point()+geom_smooth()+
  theme_bw()+
  facet_wrap(~Species, scales="free")


###parsnip_addin()### 
#Formulas <- leave_var_out_formulas(Catch ~ ., data = Embayment) 
folds <- bootstraps(Embayment, times = 10)

#### define multiple recipes with specific equations #### 

Full_Recipe<- recipe(Catch ~ ., data=Embayment) %>% 
  step_select(-Date_Time) %>% 
  step_dummy(Location, Species) %>% 
  step_normalize(all_predictors())
  

Model_Spec  <- 
  gen_additive_mod() %>% 
  set_args(select_features = TRUE) %>% 
  set_args(adjust_deg_free = tune()) %>% 
  set_engine("mgcv") %>% 
  set_mode("regression")

GAM_Models<- workflow_set(
  preproc = list(Full_Recipe),
  models = list(GAM=Model_Spec),
)

GAM_res <-
  GAM_Models %>% 
  workflow_map("fit_resamples", resamples=folds)

GAM_res




GAM_models<- workflow_set(
  add_variables(outcomes = c(Catch), predictors = ~.) %>% 
  add_model(Model_Spec, formula = Catch ~.) %>% 
  fit(data = Embayment) %>% 
  extract_fit_engine()
)


summary(GAM_Models)

summary(gamworkflow)
gam.check(gamworkflow)



plot(gamworkflow,10, shade=TRUE)


model_p <- predict_gam(gamworkflow)
model_p


model_p %>%
  ggplot(aes(Mean_K,fit)) +
  geom_smooth_ci(Species)+
  facet_wrap(~Species, scales="free")
