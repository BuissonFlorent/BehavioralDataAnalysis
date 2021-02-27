### This file analyzes the experimental data from "Are Emily and Greg More Employable Than Lakisha and Jamal? 
### A Field Experiment on Labor Market Discrimination"

##### Set-up #####

set.seed(1235)
options(scipen=10)

library(pwr)
library(tidyverse)
library(haven) #For function read_dta

library(rstudioapi)
### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

options(scipen=10)

data <- read_dta(file = "2007_Karlan_List.dta")

#Collapse binary variables into categorical variables

data <- data %>%
  mutate(group = case_when(
    control == 1 ~ "control",
    ratio == 1 ~ "treat1",
    ratio == 2 ~ "treat2",
    ratio == 3 ~ "treat3")) %>%
  mutate(group = factor(group, levels = c("control", "treat1", "treat2", "treat3"))) %>%
  mutate(gender = ifelse(female == 1, "female", "male")) %>%
  mutate(gender = ifelse(couple == 1, "couple", gender)) %>%
  mutate(state_pol =ifelse(red0 == 1, "red", "blue")) %>%
  mutate(county_pol =ifelse(redcty == 1, "red", "blue")) %>%
  select(-treatment, -control, - ratio, -female, -couple, -red0, -blue0, -redcty, -bluecty)
  
data_small <- data %>%
  select(group, gave, amount, freq, gender, state_pol, county_pol, dormant)

##### Data Analysis #####

#T-test of means for treatment 2
data_test <- data_small %>%
  filter(group %in% c("control", "treat2")) %>%
  select(group, amount) %>%
  mutate(group = droplevels(group))
t.test(amount~group, alternative = "two.sided", data=data_test)

#Linear regression
model <- lm(amount~.-gave, data = data_small)
summary(model)
