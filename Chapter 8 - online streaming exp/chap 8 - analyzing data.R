##### Setup #####
library(pwr)
library(Rlab)
library(tidyverse)
library(MBESS)

setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Part III Experimental design/Chapter 8 - online streaming exp")


set.seed(1234)
options(scipen=10)


#Reading the data
hist_data <- read_csv("chap8-historical_data.csv")
exp_data <- read_csv("chap8-experimental_data.csv")

#Formatting the data
hist_data <- hist_data %>%
  mutate(gender = factor(gender, levels = c("male", "female"))) %>%
  mutate(month = factor(month))

exp_data <- exp_data %>%
  mutate(gender = factor(gender, levels = c("male", "female"))) %>%
  mutate(oneclick = factor(oneclick)) %>%
  mutate(month = factor(month, levels = levels(hist_data$month)))


##### Power analysis #####

### Traditional statistical power analysis
library(pwr)
effect_size <- ES.h(0.194,0.184)
pwr.2p.test(h = effect_size, n = NULL, sig.level = 0.05, 
            power = 0.8, alternative = "greater")
pwr.2p.test(h = effect_size, n = NULL, sig.level = 0.1, 
            power = 0.9, alternative = "greater")


pwr.t.test(d = effect_size, n = NULL, sig.level = 0.05, 
           power = 0.8, alternative = "greater")

### Single simulation for false positives (sample size = 2000)

set.seed(123)
false_positive_single_sim <- function(s = 2000){
  prob_button <- runif(s)
  sim_data <- hist_data %>%
    sample_n(s) %>%
    mutate(oneclick = ifelse(prob_button > 0.5,1,0))
  sim_mod <- glm(booked ~ age + gender + period + oneclick, 
                 family = binomial(link = "logit"), data = sim_data)
  print(summary(sim_mod))
  return()
}
false_positive_single_sim()

### Repeated simulation for false positives (sample size = 2000, Nloop = 1000)

set.seed(1234)
false_positive_repeat_sim <- function(s = 2000, Nloop = 1000){
  
  #Simulation loop 
  sim_list <- list()
  for(i in 1:Nloop){
    sim_data <- hist_data %>%
      sample_n(s) %>%
      mutate(oneclick = ifelse(runif(s) > 0.5,1,0))
    sim_mod <- glm(booked ~ age + gender + period + oneclick, 
                   family = binomial(link = "logit"), data = sim_data)
    sim_list[[i]] <- data.frame(
      coeff = sim_mod$coefficients['oneclick'],
      pvalue = coef(summary(sim_mod))[5,4]
    )
  }
  sim_data_summary <- bind_rows(sim_list)
  print(summary(sim_data_summary))
  return(sim_data_summary)
}

sim_data_summary <- false_positive_repeat_sim(s=2000,Nloop=1000)

#Determine how large of an effect we may observe
sim_data_summary %>%
  filter(pvalue <= 0.1) %>%
  mutate(coeff = abs(coeff)) %>%
  summarize(min_eff = min(coeff),
            avg_eff = mean(coeff),
            max_eff = max(coeff))


### Repeated simulation for false negatives (sample size = 2000, Nloop = 1000)

#Add predicted probability of booking to historical data
hist_mod <- glm(booked ~ age + gender + period, family = binomial(link = "logit"), data = hist_data)
hist_data <- hist_data %>%
  mutate(pred_prob_bkg = hist_mod$fitted.values)

set.seed(1234)
false_negative_repeat_sim <- function(s = 2000, Nloop = 1000){
  #Simulation loop 
  sim_list <- list()

  for(i in 1:Nloop){
    sim_data <- hist_data %>%
      sample_n(s) %>%
      mutate(oneclick = ifelse(runif(s) > 0.5,1,0)) %>%
      mutate(pred_prob_bkg = ifelse(oneclick == 1, pred_prob_bkg + 0.01, pred_prob_bkg)) %>%
      mutate(booked = ifelse(pred_prob_bkg >= runif(s,0,1),1, 0))
    
    sim_mod <- glm(booked ~ age + gender + period + oneclick, 
                   family = binomial(link = "logit"), data = sim_data)
    sim_list[[i]] <- data.frame(
      coeff = sim_mod$coefficients['oneclick'],
      pvalue = coef(summary(sim_mod))[5,4]
    )
  }
  sim_data_summary <- bind_rows(sim_list)
  return(sim_data_summary)
}

sim_data_summary <- false_negative_repeat_sim(s = 2000, Nloop = 100)

sim_data_summary %>%
  summarize(power = sum(pvalue <= 0.1 & coeff > 0)/n())


### Statistical significance and power simulation at scale

#Add predicted probability of booking to historical data (if not done in the previous section)
hist_mod <- glm(booked ~ age + gender + period, family = binomial(link = "logit"), data = hist_data)
hist_data <- hist_data %>%
  mutate(pred_prob_bkg = hist_mod$fitted.values)

#Determining the sample sizes for the simulation
Nreps <- 100
sample_sizes <- seq(from=10000, to=40000, by=5000) %>%
  rep(Nreps)

#Power simulation function
power_sim <- function(sample_sizes){
  Nloop <- length(sample_sizes)
  #Simulation loop 
  sim_list <- list()
  
  for(i in 1:Nloop){
    #Generate random sample size
    n <- sample_sizes[i]
    
    #Generate simulated random assignment
    sim_data <- hist_data %>%
      sample_n(n, replace = TRUE) %>%
      mutate(oneclick = ifelse(runif(n) > 0.5,1,0))
    
    ##Calculating statistical significance (false positives) variables
    
    sim_mod0 <- glm(booked ~ age + gender + period + oneclick, 
                    family = binomial(link = "logit"), data = sim_data)
    #Calculating booking rates in control and treatment groups
    bkg_rate_ctrl0 <- sim_data %>% 
      filter(oneclick == 0) %>% 
      summarize(bkg_rate_ctrl = mean(booked))
    bkg_rate_ctrl0 <- as.numeric(bkg_rate_ctrl0)
    bkg_rate_treat0 <- sim_data %>% 
      filter(oneclick == 1) %>% 
      summarize(bkg_rate_treat = mean(booked)) 
    bkg_rate_treat0 <- as.numeric(bkg_rate_treat0)
    #Calculating differences in booking rates between the two groups
    bkg_rate_diff0 <- bkg_rate_treat0 - bkg_rate_ctrl0
    
    ## Test of proportion with no effect
    test0 <- with(sim_data, table(oneclick, booked)) %>%
      prop.test(alternative = "greater", conf.level = 0.9)
    test0_pvalue <- unlist(test0['p.value'])
    test0_diff <- sim_data %>% group_by(oneclick) %>% summarize(mean(booked))
    
    #Calculating statistical power (false negatives) variables
    sim_data <- sim_data %>%
      mutate(pred_prob_bkg = ifelse(oneclick == 1, pred_prob_bkg + 0.01, pred_prob_bkg)) %>%
      mutate(booked = ifelse(pred_prob_bkg >= runif(n,0,1),1, 0))
    sim_mod1 <- glm(booked ~ age + gender + period + oneclick, 
                    family = binomial(link = "logit"), data = sim_data)
    
    #Calculating booking rates in control and treatment groups
    bkg_rate_ctrl1 <- sim_data %>% 
      filter(oneclick == 0) %>% 
      summarize(bkg_rate_ctrl = mean(booked))
    bkg_rate_ctrl1 <- as.numeric(bkg_rate_ctrl1)
    bkg_rate_treat1 <- sim_data %>% 
      filter(oneclick == 1) %>% 
      summarize(bkg_rate_treat = mean(booked)) 
    bkg_rate_treat1 <- as.numeric(bkg_rate_treat1)
    #Calculating differences in booking rates between the two groups
    bkg_rate_diff1 <- bkg_rate_treat1 - bkg_rate_ctrl1
    
    ## Test of proportion with effect
    test1 <- with(sim_data, table(oneclick, booked)) %>%
      prop.test(alternative = "greater", conf.level = 0.9)
    test1_pvalue <- unlist(test1['p.value'])
    test1_diff <- sim_data %>% group_by(oneclick) %>% summarize(mean(booked))
    
    #Saving the data from this simulated experiment
    sim_list[[i]] <- data.frame(
      sample_size = n,
      coeff0 = sim_mod0$coefficients['oneclick'],
      pvalue0 = coef(summary(sim_mod0))[5,4],
      bkg_rate_ctrl0 = bkg_rate_ctrl0,
      bkg_rate_treat0 = bkg_rate_treat0, 
      bkg_rate_diff0 = bkg_rate_diff0,
      coeff1 = sim_mod1$coefficients['oneclick'],
      pvalue1 = coef(summary(sim_mod1))[5,4],
      bkg_rate_ctrl1 = bkg_rate_ctrl1,
      bkg_rate_treat1 = bkg_rate_treat1, 
      bkg_rate_diff1 = bkg_rate_diff1,
      test0_pvalue = test0_pvalue,
      test1_pvalue = test1_pvalue
    )
  }
  sim_data_summary <- bind_rows(sim_list)
  return(sim_data_summary)
}

set.seed(1234)
sim_data_summary <- power_sim(sample_sizes)
summary(sim_data_summary)

#Determining statistical power of test of proportions
test_results1 <- sim_data_summary %>%
  group_by(sample_size) %>%
  summarize(power = sum(test1_pvalue < 0.1 & bkg_rate_diff1 >0)/n())
test_results1

#Determining statistical significance of test of proportions
test_results0 <- sim_data_summary %>%
  group_by(sample_size) %>%
  summarize(significance = sum(test0_pvalue < 0.1)/n())
test_results0

#Determining statistical significance of regression
results0 <- sim_data_summary %>%
  group_by(sample_size) %>%
  summarize(significance = sum(pvalue0 < 0.1 & coeff0 > 0)/n())
results0

#Determining statistical power of regression
results1 <- sim_data_summary %>%
  mutate(coeff = abs(coeff1)) %>%
  group_by(sample_size) %>%
  summarize(power = sum(pvalue1 < 0.1 & coeff1 > 0)/n())
results1

#Plotting results
par(mfrow=c(1,2))
with(test_results1, plot(sample_size, power, main = "empirical power for test of proportion", ylim=c(0,1)))
abline(h=0.9, col='red')
with(results1, plot(sample_size, power, main = "empirical power for logistic regression", ylim=c(0,1)))
abline(h=0.9, col='red')
par(new = F)



##### Analyzing the results of the experiment #####

### Booking probability

tab <- with(exp_data, table(oneclick, booked))
prop.test(tab, alternative = "greater", conf.level = 0.9)
exp_data %>% group_by(oneclick) %>% summarise(prop = mean(booked))

log_mod_exp <- glm(booked ~ oneclick + age + gender, 
                   data = exp_data, family = binomial(link = "logit"))
summary(log_mod_exp)

### Calculating average difference in probabilities
no_button <- exp_data %>% 
  mutate(oneclick = 0) %>% 
  #mutate(oneclick = factor(oneclick)) %>%
  select(age, gender, oneclick)
button <- exp_data %>% 
  mutate(oneclick = 1) %>% 
  #mutate(oneclick = factor(oneclick)) %>%
  select(age, gender, oneclick)
#Adding the predictions of the model 
no_button <- no_button %>%
  mutate(pred_mod = predict(object=log_mod_exp, newdata = no_button, type="response"))
button <- button %>%
  mutate(pred_mod = predict(object=log_mod_exp, newdata = button, type="response"))
#Calculating average difference in probabilities
diff <- button$pred_mod - no_button$pred_mod
mean(diff)
