#################################
##### This script analyzes the data used in chapter 12, 
##### Mediation and Instrumental Variables
#################################

##### Setup #####

# Common libraries
suppressMessages(suppressWarnings(library(tidyverse)))
library(boot) #Required for Bootstrap simulations
library(rstudioapi) #To load data from local folder
library(ggpubr) #To generate multi-plots

# Chapter-specific libraries
library(ivreg) # for IV regressions

### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

set.seed(1234)
options(scipen=10)

#Loading the data from the chapter on moderation
hist_data <- read_csv("chap11-historical_data.csv")

#Formatting the data
hist_data <- hist_data %>%
  mutate(store_id = factor(store_id)) %>%
  mutate(day = factor(day)) %>%
  mutate(children = factor(children)) %>%
  mutate(play_area = factor(play_area))

##### 1. Mediation #####

#### 1.1. Definition and Measurement ####

summary(lm(duration~play_area, data=hist_data))
summary(lm(groceries_purchases~play_area, data=hist_data))
summary(lm(groceries_purchases~duration, data=hist_data))
summary(lm(groceries_purchases~duration+play_area, data=hist_data))

### Bootstrap CI for percentage mediated 
percentage_mediated_fun <- function(dat){
  total_effect <- summary(lm(groceries_purchases~play_area, data=dat))$coefficients['play_area1', 'Estimate']
  coeff_med1 <- summary(lm(duration~play_area, data=dat))$coefficients['play_area1', 'Estimate']
  coeff_med2 <- summary(lm(groceries_purchases~duration, data=dat))$coefficients['duration', 'Estimate']
  mediated_effect <- coeff_med1 * coeff_med2
  percentage_mediated <- mediated_effect / total_effect
  return(percentage_mediated)
}
percentage_mediated_fun(hist_data)

boot_CI_fun <- function(dat, metric_fun, B = 20){

  boot_metric_fun <- function(dat, J){
    boot_dat <- dat[J,]
    return(metric_fun(boot_dat))
  }
  boot.out <- boot(data=dat, statistic=boot_metric_fun, R=B)
  confint <- boot.ci(boot.out, conf = 0.90, type = c('perc'))
  CI <- confint$percent[c(4,5)]
  
  return(CI)
}
boot_CI_fun(hist_data, percentage_mediated_fun)

##### 2. Instrumental variables #####

#Loading the experimental data from chapter 9
exp_data <- read_csv("chap9-experimental_data.csv")

#Reformating the data
exp_data <- exp_data %>%
  mutate(center_ID = factor(center_ID)) %>%
  mutate(rep_ID = factor(rep_ID)) %>%
  mutate(reason = factor(reason)) %>%
  mutate(group = factor(group)) 

#Reduced regression, coeff = 1.6
red_mod <- lm(M6Spend~group+age, data=exp_data)
summary(red_mod)

#First stage regression, coeff = 0.5
S1_mod <- lm(call_CSAT~group+age, data=exp_data)
summary(S1_mod)

#Baseline (biased) regression
lm_mod <- lm(M6Spend~call_CSAT+age, data=exp_data)
summary(lm_mod)

### IV regression
iv_mod <- ivreg::ivreg(M6Spend~call_CSAT + age | group + age, data=exp_data)
summary(iv_mod)

iv_metric_fun <- function(dat){
  iv_mod <- ivreg::ivreg(M6Spend~call_CSAT + age | group + age, data=dat)
  iv_summ <- summary(iv_mod)
  coeff <- iv_summ$coefficients['call_CSAT','Estimate']
  return(coeff)
}
boot_CI <- boot_CI_fun(exp_data, iv_metric_fun)