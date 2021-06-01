##### Setup #####
library(tidyverse)
library(blockTools)
library(Rlab) # for function rbern
library(scales) # for function rescale

library(rstudioapi)
### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

options(scipen=10)
set.seed(3)

##### Functions #####

#Generating the list of centers and reps with the corresponding average CSAT
reps_gen_fct <- function(Ncenters = 10, Nreps_per_center = 20, Nreps_per_center_sigma = 2){
  set.seed(1234)
  #Generating the centers
  Nreps <- round(rnorm(Ncenters, Nreps_per_center, Nreps_per_center_sigma),0)
  centers_CSAT_mean <- rnorm(Ncenters, 4, 1)
  centers_CSAT_sd <- rnorm(Ncenters, 1, 0.3)
  
  #Generating the reps
  reps_list <- list()
  for(i in 1:Ncenters){
    reps_CSAT_coeff <- rnorm(Nreps[i],centers_CSAT_mean[i],centers_CSAT_sd[i])
    reps_list[[i]] <- data.frame(
      center_ID = i,
      rep_CSAT = reps_CSAT_coeff)
  }
  reps <- bind_rows(reps_list)
  
  #Adding rep ID
  reps <- reps %>%
    mutate(rep_ID = 1:nrow(reps)) %>%
    mutate(center_ID = as.factor(center_ID)) %>%
    mutate(rep_ID = as.factor(rep_ID))
  
  return(reps)
}

#Generating the historical data
prel_data_gen_fct <- function(reps, Ncalls_rep = 3600){
  
  calls_list <- list()
  for(i in 1:nrow(reps)){
    Ncalls <- round(rnorm(1,Ncalls_rep, 50),0)
    age <- round(runif(Ncalls, 20,60), 0)
    coeff_age <- rnorm(Ncalls, 0.02, 0.005)
    age_to_reason <- age * 0.01
    reason <- sapply(age_to_reason, function(x) rbern(1,x))
    coeff_reason <- rnorm(Ncalls, 0.2, 0.05)
    openness <- rnorm(Ncalls, 0, 1.5)
    coeff_openness <- rnorm(Ncalls, 0.2, 0.05)
    month <- sample(c(1,2,3), Ncalls, replace = TRUE)
    epsilon <- rnorm(Ncalls, 0, 0.5)
    
    calls_list[[i]] <- data.frame(
      center_ID = reps$center_ID[i],
      rep_ID = reps$rep_ID[i],
      rep_CSAT = reps$rep_CSAT[i],
      age = age,
      coeff_age = coeff_age,
      reason = reason,
      coeff_reason = coeff_reason,
      openness = openness,
      coeff_openness = coeff_openness,
      month = month,
      epsilon = epsilon
    )
  }
  calls <- bind_rows(calls_list)
  
  #Adding call CSAT
  calls <- calls %>%
    mutate(call_CSAT = rep_CSAT + reason * coeff_reason + age * coeff_age + openness * coeff_openness + epsilon)
  
  #Bounding values between zero and ten
  calls <- calls %>%
    mutate(call_CSAT = ifelse(call_CSAT < 0, 0, call_CSAT)) %>%
    mutate(call_CSAT = ifelse(call_CSAT > 10, 10, call_CSAT))
  
  #Converting reason to factor
  calls <- calls %>%
    mutate(reason = factor(reason, labels = c("payment", "property")))
  #Converting center and rep to factor
  calls <- calls %>%
    mutate(center_ID = factor(center_ID)) %>%
    mutate(rep_ID = factor(rep_ID))
  
  # Adding variable for 6-month spend following a booking
  #simulating the values of the booking probability
  N <- nrow(calls)
  intercept <- rlnorm(N, sdlog = 0.5) * 100
  beta_a <- - rnorm(N,3,1)
  beta_o <- rnorm(N, 10, 3)
  beta_c <- rnorm(N, 5, 1.5) 
  calls <- calls %>%
    mutate(M6Spend = intercept + beta_a * age + beta_o * openness + beta_c * call_CSAT) %>%
    mutate(M6Spend = pmax(M6Spend,0))
  
  #Determining true coefficients of regression: beta_c = 2.932
  print(summary(lm(M6Spend ~ age + openness + call_CSAT, data = calls)))
  
  #Removing scaffolding variables
  calls <- calls %>%
    select(-epsilon, -coeff_reason,-rep_CSAT, -coeff_age, -coeff_openness, -openness)
  
  return(calls)
}

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

#Generating experimental data
exp_data_gen_fct <- function(reps, Ncalls_rep, blocked_assgt, effect = 1){
  reps <- reps %>%
    full_join(blocked_assgt, by="center_ID")
  
  calls_list <- list()
  for(i in 1:nrow(reps)){
    Ncalls <- round(rnorm(1,Ncalls_rep, 10),0)
    age <- round(runif(Ncalls, 20,60), 0)
    coeff_age <- rnorm(Ncalls, 0.02, 0.005)
    age_to_reason <- age * 0.01
    reason <- sapply(age_to_reason, function(x) rbern(1,x))
    coeff_reason <- rnorm(Ncalls, 0.2, 0.05)
    openness <- rnorm(Ncalls, 0, 1.5)
    coeff_openness <- rnorm(Ncalls, 0.2, 0.05)
    month <- 4
    epsilon <- rnorm(Ncalls, 0, 0.5)
    
    calls_list[[i]] <- data.frame(
      center_ID = reps$center_ID[i],
      group = reps$group[i],
      rep_ID = reps$rep_ID[i],
      rep_CSAT = reps$rep_CSAT[i],
      age = age,
      coeff_age = coeff_age,
      reason = reason,
      coeff_reason = coeff_reason,
      openness = openness,
      coeff_openness = coeff_openness,
      month = month,
      epsilon = epsilon
    )
  }
  calls <- bind_rows(calls_list)
  
  #Adding call CSAT
  calls <- calls %>%
    mutate(call_CSAT = rep_CSAT + reason * coeff_reason + age * coeff_age + openness * coeff_openness + epsilon) %>%
    mutate(call_CSAT = call_CSAT + ifelse(group == "treat", effect,0))
  
  #Bounding values between zero and ten
  calls <- calls %>%
    mutate(call_CSAT = ifelse(call_CSAT < 0, 0, call_CSAT)) %>%
    mutate(call_CSAT = ifelse(call_CSAT > 10, 10, call_CSAT))
  #hist(calls$call_CSAT)
  
  #Converting reason to factor
  calls <- calls %>%
    mutate(reason = factor(reason, labels = c("payment", "property")))
  
  # Adding variable for 6-month spend following a booking
  #simulating the values of the booking probability
  N <- nrow(calls)
  intercept <- rlnorm(N, sdlog = 0.5) * 100
  beta_a <- - rnorm(N,3,1)
  beta_o <- rnorm(N, 10, 3)
  beta_c <- rnorm(N, 5, 1.5) 
  calls <- calls %>%
    mutate(M6Spend = intercept + beta_a * age + beta_o * openness + beta_c * call_CSAT) %>%
    mutate(M6Spend = pmax(M6Spend,0))
  
  #Determining true coefficients of regression: beta_c = 2.876
  print(summary(lm(M6Spend ~ age + openness + call_CSAT, data = calls)))
  
  #Removing scaffolding variables
  calls <- calls %>%
    select(-epsilon, -coeff_reason,-rep_CSAT, -coeff_age, -coeff_openness, -openness)
  
  return(calls)
  
}


##### Data generation #####

reps <- reps_gen_fct()

hist_data <- prel_data_gen_fct(reps)

#summary(lm(M6Spend~age+openness+call_CSAT, data=hist_data))

blocked_assgt <- blocking_fct(hist_data)

exp_data <- exp_data_gen_fct(reps, Ncalls_rep = 1200, blocked_assgt, effect = 1)

write_csv(hist_data, "chap10-historical_data.csv")
write_csv(exp_data, "chap10-experimental_data.csv")


