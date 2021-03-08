##### Setup #####
library(pwr)
library(Rlab)
library(tidyverse)
library(MBESS)

setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Part III Experimental design/Chapter 9 - stratified randomization")
options(scipen=10)
set.seed(4)


##### Generating the baseline data #####

Nperiods <- 36 #Number of months
N <- 5000 #Number of properties

data_generation_fct <- function(Nperiods, N){
  #Simulating the variables that are constant at the property level
  ID <- 1:N
  sq_ft <- rnorm(N, 800, 100)
  tier <- sample(c(1,2,2,3,3,3,3), size = N, replace = TRUE)
  avg_review <- rnorm(N, 7, 1.5) %>%
    pmax(rep(0,N)) %>%
    pmin(rep(10,N))

  dat_list <- list()
  for(i in 1:Nperiods){
    #simulating the values of the explanatory variables
    period <- i
    month <- period %% 12 + 1
    seasonality <- (1 + sin(i * 3.1415 * 2 / 12))/2

    #Simulating the values of the target behavior
    intercept <- rnorm(N,10,3)
    beta_s <- rnorm(N,7,2)
    beta_sq <- rnorm(N,0.009, 0.01)
    beta_t1 <- rpois(N,7)
    beta_t2 <- rpois(N,4)
    beta_t3 <- rpois(N,1)
    beta_r <- rpois(N,2)
    e <- rnorm(N,0,5)
    
    BPday <- intercept + beta_s * seasonality + beta_sq * sq_ft + beta_t1 * (tier == 1) + 
      beta_t2 * (tier == 2) + beta_t3 * (tier == 3) + beta_r * avg_review + e 
    BPday <- BPday %>%
      pmax(rep(0,N))
    
    dat_list[[i]] <- data.frame(
      ID = ID,
      period = period,
      month = month,
      sq_ft = sq_ft,
      tier = tier, 
      avg_review = avg_review,
      BPday = BPday
    )
  }

  dat <- bind_rows(dat_list)
  dat <- dat %>%
    mutate(tier = factor(tier))
  
  return(dat)
}

dat <- data_generation_fct(Nperiods, N)
summary(dat)

exp_data <- dat %>%
  filter(period == 36)

hist_data <- dat %>%
  filter(period != 36)
  


##### Blocking experimental groups #####
library(blockTools)
library(caret)
library(scales)

#Sample size for experiment (divisible by 3)
Nexp <- 1500

### Prepping the data
block_prep_fun <- function(dat){
  #Extracting property-level variables
  dat <- dat %>%
    group_by(ID, tier) %>%
    summarise(sq_ft = mean(sq_ft),
              avg_review = mean(avg_review),
              BPday = mean(BPday)) %>%
    ungroup()
  
  #Isolating the different components of our data
  ID <- dat$ID  # Owner identifier
  dat <- dat %>% select(-ID)
  cat_vars <- dat %>%
    select_if(is.factor) #Selecting categorical variables
  num_vars <- dat %>%
    select_if(function(x) is.numeric(x)|is.integer(x)) #Selecting numeric variables
  
  #One-hot encoding categorical variables
  cat_vars_out <- data.frame(predict(dummyVars(" ~.", data=cat_vars), newdata = cat_vars))
  
  #Normalizing numeric variables
  num_vars_out <- num_vars %>%
    mutate_all(rescale)
  
  #Putting the variables back together
  dat_out <- cbind(ID, num_vars_out, cat_vars_out)
}

prepped_data <- block_prep_fun(hist_data)

blocking_wrapper_fun <- function(dat, Nexp){
  
  #Extracting a sample of the right size
  dat <- dat %>% slice_sample(n=Nexp)
  
  #Getting stratified assignment
  assgt <- dat %>%
    block(dat, id.vars = c("ID"), n.tr = 3, 
          algorithm = "naiveGreedy", distance = "euclidean") %>%
    assignment() 
  assgt <- assgt$assg$`1` 
  
  assgt <- assgt %>%
    select(-'Max Distance')
  colnames(assgt) <- c("ctrl", "treat1","treat2")
  assgt_long <- gather(assgt,group, ID, 'ctrl':'treat2') %>%
    mutate(group = as.factor(group)) %>%
    mutate(ID = as.integer(ID))
  
  dat_final <- full_join(dat, assgt_long, by="ID")
  
  return(dat_final)
}

##### Adding treatment effect to experiment #####

#Stratifying the experimental data
exp_data_out <- exp_data %>%
  blocking_wrapper_fun(Nexp)

#Assigning treatment effect
exp_data_out <- exp_data_out %>%
  mutate(compliant = ifelse(runif(Nexp)>= 0.8,1,0)) %>%
  mutate(compliant = ifelse(group == "ctrl",1, compliant)) %>%
    mutate(BPday_strat = case_when(
    (group == "ctrl" | compliant == 0) ~ BPday + rnorm(Nexp, 0.5, 0.1),
    (group == "treat2" & compliant == 1) ~ BPday + rnorm(Nexp, 8.5, 2),
    (group == "treat1" & compliant == 1) ~ BPday + rnorm(Nexp, 3.6, 0.3)
  ))
exp_data_out <- exp_data_out %>%
  select(-BPday) %>%
  rename(BPday = BPday_strat)

exp_data_out %>%
  group_by(group, compliant) %>%
  summarise(cnt = n(),
            avg_BP = mean(BPday))
           
write_csv(hist_data, "chap9-historical_data.csv")
write_csv(exp_data_out, "chap9-experimental_data.csv")          
           
           





