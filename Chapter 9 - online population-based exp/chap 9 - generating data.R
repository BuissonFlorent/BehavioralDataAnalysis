##### Setup #####
library(pwr)
library(Rlab)
library(tidyverse)
library(MBESS)

setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Part III Experimental design/Chapter 9 - online population-based exp")
options(scipen=10)


##### Generating the baseline data #####

Nperiods <- 36 #Number of months
N <- 5000 #Number of properties

set.seed(1234)
data_generation_fct <- function(Nperiods, N){
  #Simulating the variables that are constant at the property level
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

N <- 2001

blocking_fct <- function(dat, N){
  block_data <- dat %>%
    select(-period, -month) %>%
    sample_n(N) %>%
    #Creating an ID variable for bookkeeping
    mutate(id = as.character(seq(1,N)))
  
  #Isolating the different components of our data
  id <- block_data$id  #Customer identifier
  cat_vars <- block_data %>%
    select_if(is.factor) #Selecting categorical variables
  num_vars <- block_data %>%
    select_if(function(x) is.numeric(x)|is.integer(x)) #Selecting numeric variables
  
  #One-hot encoding categorical variables
  cat_vars2 <- data.frame(predict(dummyVars(" ~.", data=cat_vars), newdata = cat_vars))
  
  #Normalizing numeric variables
  num_vars2 <- num_vars %>%
    mutate_all(rescale)
  
  #Putting the variables back together
  block_data2 <- cbind(id, num_vars2, cat_vars2)
  
  #Getting stratified experimental group assignment
  assgt <- block_data2 %>%
    block(block_data2, id.vars = c("id"), n.tr = 3, 
          algorithm = "naiveGreedy", distance = "euclidean") %>%
    assignment() 
  assgt <- assgt$assg$`1` 
  assgt <- assgt %>%
    select(-'Max Distance')
  colnames(assgt) <- c("ctrl", "treat1","treat2")
  assgt_long <- gather(assgt,group, id, 'ctrl':'treat2') %>%
    mutate(group = as.factor(group))
  
  dat_final <- full_join(block_data, assgt_long, by="id")
  
  return(dat_final)
}

##### Adding treatment effect to experiment #####

#Stratifying the experimental data
exp_data2 <- exp_data %>%
  blocking_fct(N)

#Assigning treatment effect
exp_data2 <- exp_data2 %>%
  mutate(BPday_strat = case_when(
    group == "ctrl" ~ BPday + rnorm(N, 0.5, 1),
    group == "treat2" ~ BPday + rnorm(N, 12, 2),
    group == "treat1" ~ BPday + rnorm(N, 1.5, 1)
  ))
exp_data2 <- exp_data2 %>%
  select(-BPday) %>%
  rename(BPday = BPday_strat)
           
write_csv(hist_data, "chap9-historical_data.csv")
write_csv(exp_data2, "chap9-experimental_data.csv")          
           
           
     




