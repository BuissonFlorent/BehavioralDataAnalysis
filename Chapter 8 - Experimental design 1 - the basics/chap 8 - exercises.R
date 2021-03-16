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


data <- read_dta(file = "2004_Bertrand_Mullainathan.dta")

#Reframing experimental variable as factor
data_small <- data %>%
  mutate(firstname = factor(firstname)) %>%
  mutate(race = factor(race, levels = c('w','b'))) %>%
  mutate(sex = factor(sex, levels = c('m','f'))) %>%
  mutate(col = factor(col, levels = c(0,1))) %>%
  select(call, education, yearsexp, race, sex, col)




##### Data analysis #####

#Test of proportions
tab <- with(data_small, table(race, call))
prop.test(tab, alternative = "less", conf.level = 0.9)
data_small %>% group_by(race) %>% summarise(prop = mean(call))


#Logistic regression
model <- glm(call~education+yearsexp+col+race+sex, family = binomial(link = 'logit'),data = data_small)
summary(model)
