#################################
##### This script analyzes the data used in chapter 12, 
##### Mediation and Instrumental Variables
#################################

##### Setup #####

#Common libraries
library(tidyverse)
library(ggpubr)
library(boot)
library(rstudioapi)
### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

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

boot_CI_fun <- function(dat, metric_fun, B = 100){

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

#### 1.4. Reducing Uncertainty in Coefficients ####

#Measuring the overall aggregate effect
agg_effect_fun <- function(dat){
  total_effect <- summary(lm(groceries_purchases~play_area, 
                             data=dat))$coefficients['play_area1', 'Estimate']
  return(total_effect)
}

#Measuring the total effect through its components
composed_effect_fun <- function(dat){
  coeff_med1 <- summary(lm(duration~play_area, 
                           data=dat))$coefficients['play_area1', 'Estimate']
  coeff_med2 <- summary(lm(groceries_purchases~duration, 
                           data=dat))$coefficients['duration', 'Estimate']
  mediated_effect <- coeff_med1 * coeff_med2
  direct_effect <- summary(lm(groceries_purchases~play_area+duration, 
                              data=dat))$coefficients['play_area1', 'Estimate']
  total_effect <- mediated_effect + direct_effect
  return(mediated_effect)
}

#Creating a smaller sample for increased variability
hist_data_sample <- hist_data %>%
  group_by(store_id) %>%
  slice_sample(n=25)

agg_CI <- boot_CI_fun(hist_data_sample, agg_effect_fun)
composed_CI <- boot_CI_fun(hist_data_sample, composed_effect_fun)
agg_CI
composed_CI

##### 2. Instrumental variables #####

#Common libraries
library(tidyverse)
library(ggpubr)
library(boot)

#Libraries for IV regressions
library(ivreg)
library(ivprobit)

#Loading the data from the chapter 10, then setting the right folder
setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Part III Experimental design/Chapter 10 - cluster randomization and hierarchical modeling")
hist_data <- read_csv("chap10-historical_data.csv")
exp_data <- read_csv("chap10-experimental_data.csv")
setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Part IV Advanced tools/Chapter 12 - Mediation and Instrumental Variables")

#Reformating the data
hist_data <- hist_data %>%
  mutate(center_ID = factor(center_ID)) %>%
  mutate(rep_ID = factor(rep_ID)) %>%
  mutate(reason = factor(reason))
exp_data <- exp_data %>%
  mutate(center_ID = factor(center_ID, levels = levels(hist_data$center_ID))) %>%
  mutate(rep_ID = factor(rep_ID, levels = levels(hist_data$rep_ID))) %>%
  mutate(reason = factor(reason, levels = levels(hist_data$reason))) %>%
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




# IV regression for binary effect (probit regression)
log_data <- exp_data %>%
  mutate(spend_bin = ifelse(M6Spend >= 50, 1, 0)) %>%
  #mutate(spend_bin = factor(spend_bin, levels=c(0,1))) %>%
  select(group, spend_bin, call_CSAT, age)

#summary(glm(spend_bin~call_CSAT+age, data=log_data, family = binomial(link = "logit")))
summary(glm(spend_bin~call_CSAT+age, data=log_data, family = binomial(link = "probit")))
summary(glm(spend_bin~group, data=log_data, family = binomial(link = "probit")))
summary(glm(spend_bin~call_CSAT, data=log_data, family = binomial(link = "probit")))

summary(ivprobit(spend_bin~age|call_CSAT|group, data=log_data))
