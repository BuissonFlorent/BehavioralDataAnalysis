##### Setup #####
library(tidyverse)
library(Rlab)
library(lme4)
library(lmerTest)
library(blockTools)
library(caret)
library(scales)


setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Part III Experimental design/Chapter 10 - offline population-based exp")
options(scipen=10)

#Reading the data
hist_data <- read_csv("chap10-historical_data.csv")
exp_data <- read_csv("chap10-experimental_data.csv")

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

##### Functions #####

#Generating the stratified random assignment by call center
blocking_fct <- function(prel_data){
  assgt_data <- prel_data %>%
    select(-age,-reason) %>%
    #Adding number of reps in each call center
    group_by(center_ID, rep_ID) %>%
    mutate(nreps = n()) %>%
    ungroup() %>%
    #Adding average call CSAT per call center
    group_by(center_ID) %>%
    #Averaging over constant values for nreps, just to keep the number of reps in the summary
    summarize(nreps = mean(nreps),
              avg_call_CSAT = mean(call_CSAT)) %>%
    #Rescaling variables
    mutate(nreps = rescale(nreps),
           avg_call_CSAT = rescale(avg_call_CSAT))
  assgt <- assgt_data %>%
    block( id.vars = c("center_ID"), n.tr = 2, 
           algorithm = "naiveGreedy", distance = "euclidean") %>%
    assignment() 
  assgt <- assgt$assg$`1` 
  assgt <- assgt %>%
    select(-Distance)
  colnames(assgt) <- c("ctrl", "treat")
  assgt <- gather(assgt,group, center_ID, 'ctrl':'treat')%>%
    mutate(group = as.factor(group)) %>%
    mutate(center_ID = factor(center_ID, levels = levels(assgt_data$center_ID)))
  
  dat_final <- full_join(assgt_data, assgt, by="center_ID") %>%
    select(-nreps,-avg_call_CSAT)
  
  return(dat_final)
}

#Generating simulated data
sim_data_gen_fct <- function(reps, prel_data, Ncalls_rep = 250, Nloops = 10, effect = 0.5){
  options(warn=-1)
  sim_list <- list()
  
  for(i in 1:Nloops){
    if(i%% 10 == 0) cat("Iteration number", i, "\n")
    
    #Generating random assignment
    blocked_assgt <- blocking_fct(prel_data) 
    
    #Simulating experimental data with no true effect
    sim_data <- prel_data %>%
      #Sampling historical data at the rep level
      group_by(center_ID, rep_ID) %>% sample_n(Ncalls_rep, replace = TRUE) %>% ungroup() %>%
      #Assigning experimental group 
      full_join(blocked_assgt, by = "center_ID")
      
    #Estimating treatment coefficient and p-value with standard regression
    lm_mod0 <- lm(call_CSAT ~ age + reason + group + center_ID + rep_ID, data = sim_data)
    
    lm_est0_coeff <- summary(lm_mod0)$coefficients[4,1]
    lm_est0_pvalue <- summary(lm_mod0)$coefficients[4,4]
    
    #Estimating treatment coefficient and p-value with hierarchical regression
    h_mod0 <- lmer(data=sim_data, call_CSAT ~ reason + age + group + (1|center_ID/rep_ID))
    
    h_est0_coeff <- fixef(h_mod0)["grouptreat"]
    h_est0_pvalue <- summary(h_mod0)$coefficients[4,5]
    
    #Adding true effect
    sim_data <- sim_data %>%
      mutate(call_CSAT = call_CSAT + ifelse(group == "treat", effect,0)) %>%
      mutate(call_CSAT = ifelse(call_CSAT < 0, 0, call_CSAT)) %>%
      mutate(call_CSAT = ifelse(call_CSAT > 10, 10, call_CSAT))
    
    #Estimating treatment coefficient and p-value with standard regression
    lm_mod1 <- lm(call_CSAT ~ age + reason + group + center_ID + rep_ID, data = sim_data)
    
    lm_est1_coeff <- summary(lm_mod1)$coefficients[4,1]
    lm_est1_pvalue <- summary(lm_mod1)$coefficients[4,4]
    
    #Estimating treatment coefficient and p-value with hierarchical regression
    h_mod1 <- lmer(data=sim_data, call_CSAT ~ reason + age + group + (1|center_ID/rep_ID))
    
    h_est1_coeff <- fixef(h_mod1)["grouptreat"]
    h_est1_pvalue <- summary(h_mod1)$coefficients[4,5]
    
    
    #Gathering results
    sim_list[[i]] <- data.frame(
      #Coefficients and p-values for null effect
      lm_est0_coeff = lm_est0_coeff,
      lm_est0_pvalue = lm_est0_pvalue,
      h_est0_coeff = h_est0_coeff,
      h_est0_pvalue = h_est0_pvalue,
      #Coefficients and p-values for true effect
      lm_est1_coeff = lm_est1_coeff,
      lm_est1_pvalue = lm_est1_pvalue,
      h_est1_coeff = h_est1_coeff,
      h_est1_pvalue = h_est1_pvalue
    )
  }
  sim_data_summary <- bind_rows(sim_list)
  options(warn=0)
  return(sim_data_summary)
}

##### Power analysis #####

#Statistical power analysis
library(pwr)
s <- sd(hist_data$call_CSAT)

pwr.t.test(d = 0.24, n = 198528/2, sig.level = 0.1, power = NULL, alternative = "greater")
pwr.t.test(d = 0.24, n = 5000/2, sig.level = 0.1, power = NULL, alternative = "greater")

### Simulated power analysis

sim_data_summary_0.5_1000 <- sim_data_gen_fct(reps, hist_data, Ncalls_rep = 1000, Nloops = 100, effect = 0.5)
summary(sim_data_summary)
par(mfrow=c(2, 4))
hist(sim_data_summary$lm_est0_coeff, xlim = c(-5, 5))
hist(sim_data_summary$lm_est0_pvalue)
hist(sim_data_summary$h_est0_coeff, xlim = c(-1, 1.2))
hist(sim_data_summary$h_est0_pvalue, xlim = c(0, 1))
hist(sim_data_summary$lm_est1_coeff, xlim = c(-5, 5))
hist(sim_data_summary$lm_est1_pvalue)
hist(sim_data_summary$h_est1_coeff, xlim = c(-1, 1.2))
hist(sim_data_summary$h_est1_pvalue, xlim = c(0, 1))

#Plotting empirical statistical significance
par(mfrow=c(1,2))
with(sim_data_summary, plot(lm_est0_coeff, lm_est0_pvalue, ylim=c(0,1),
                            main = "empirical stat. sig. with standard model", 
                            xlab = "estimated coefficient for treatment", 
                            ylab = "estimated p-value"))
abline(h=0.1, col='red')  
with(sim_data_summary, plot(h_est0_coeff, h_est0_pvalue, ylim=c(0,1),
                            main = "empirical stat. sig. with hierarchical model", 
                            xlab = "estimated coefficient for treatment", 
                            ylab = "estimated p-value"))
abline(h=0.1, col='red')
par(new = F)

#Plotting empirical statistical power
par(mfrow=c(1,2))
with(sim_data_summary, plot(lm_est1_coeff, lm_est1_pvalue, ylim=c(0,1),
                            main = "empirical stat. power with standard model", 
                            xlab = "estimated coefficient for treatment", 
                            ylab = "estimated p-value"))
abline(h=0.1, col='red')  
with(sim_data_summary, plot(h_est1_coeff, h_est1_pvalue, ylim=c(0,1),
                            main = "empirical stat. power with hierarchical model", 
                            xlab = "estimated coefficient for treatment", 
                            ylab = "estimated p-value"))
abline(h=0.1, col='red')
par(new = F)

#Determining statistical power of models
sim_data_summary %>%
  summarize(lm_coeff_pow = sum(lm_est1_pvalue < 0.1 & lm_est1_coeff > 0)/n(),
            h_coeff_pow = sum(h_est1_pvalue < 0.1 & h_est1_coeff > 0)/n())



##### Analyzing historical data #####

library(lme4)
library(lmerTest)
h_mod <- lmer(data=hist_data, call_CSAT ~ reason + age + (1|center_ID))
summary(h_mod)

#Checking the standard deviation of data between call centers
hist_data %>%
  group_by(center_ID)%>%
  summarize(call_CSAT = mean(call_CSAT)) %>%
  summarize(sd = sd(call_CSAT))

##### Simulating new data #####

#Sampling the data
set.seed(1234)
ptm <- proc.time()
sim_list <- list()

#Simulation loop 
Nloop <- 200
for(i in 1:Nloop){
  cat("Loop number ", i, " of ", Nloop, "\n")
  true_effect <- 1
  
  sim_data <- hist_data %>%
    #Create random allocation 
    group_by(center_ID) %>%
    mutate(grp_key = sample(c(1:20), 1, replace = FALSE)) %>%
    mutate(group = ifelse(grp_key <= 10, "c", "t")) %>%
    ungroup() %>%
    #Sample 200 calls per rep
    group_by(center_ID, rep_ID) %>%
    sample_n(400, replace = TRUE) %>%
    ungroup %>%
    #Add the impact of the treatment
    mutate(call_CSAT = call_CSAT + ifelse(group == "t", true_effect,0)) %>%
    mutate(call_CSAT = ifelse(call_CSAT > 10, 10, call_CSAT)) %>%
    mutate(group = factor(group))
  
  #Estimated coefficient for treatment with standard regression
  mod <- lm(call_CSAT ~ age + reason + group + center_ID + rep_ID, data = sim_data)
  #head(summary(mod)$coefficients, 5)
  
  lm_est <- summary(mod)$coefficients[4,1]
  
  h_mod0 <- lmer(data=sim_data, call_CSAT ~ reason + age + group + (1|center_ID/rep_ID))
  #summary(h_mod0)
  
  lmer_est <- fixef(h_mod0)["groupt"]
  
  sim_list[[i]] <- data.frame(
    true_effect = true_effect,
    lm_est = lm_est,
    lmer_est = lmer_est
  )
}
sim_data_summary <- bind_rows(sim_list)
proc.time() - ptm

head(sim_data_summary)
summary(sim_data_summary)

#Look at results
#Plotting relationship in whole dataset
par(mfrow=c(2,1))
hist(sim_data_summary$lm_est, 
     breaks = seq(from = -5, to = 5, by = 0.5), 
     main = "Distribution of estimated coefficients for standard linear model", 
     xlab = "Estimated treatment coefficient",
     xlim = c(-8, 8))
abline(v=0.5, col='red')
hist(sim_data_summary$lmer_est, 
     breaks = seq(from = -5, to = 5, by = 0.5), 
     main = "Distribution of estimated coefficients for hierarchical linear model", 
     xlab = "Estimated treatment coefficient",
     xlim = c(-8, 8))
abline(v=0.5, col='red')
par(new = F)

#Saving data here for future analyses
write.csv(sim_data, "sim_data.csv")

#Adding decision criteria
sim_data_summary <- sim_data_summary %>%
  mutate(lm_TP = ifelse(true_effect >= 0.5 & lm_est >= 0.5,1,0)) %>%
  mutate(lm_FP = ifelse(true_effect < 0.5 & lm_est >= 0.5,1,0)) %>%
  mutate(lm_TN = ifelse(true_effect < 0.5 & lm_est < 0.5,1,0)) %>%
  mutate(lm_FN = ifelse(true_effect >= 0.5 & lm_est < 0.5,1,0)) %>%
  mutate(lmer_TP = ifelse(true_effect >= 0.5 & lmer_est >= 0.5,1,0)) %>%
  mutate(lmer_FP = ifelse(true_effect < 0.5 & lmer_est >= 0.5,1,0)) %>%
  mutate(lmer_TN = ifelse(true_effect < 0.5 & lmer_est < 0.5,1,0)) %>%
  mutate(lmer_FN = ifelse(true_effect >= 0.5 & lmer_est < 0.5,1,0))

with(sim_data,table(lm_TP, lmer_TP))




 




##### Analyzing experimental data #####

h_mod <- lmer(data=exp_data, call_CSAT ~ reason + age + group + (1|center_ID))
summary(h_mod)
