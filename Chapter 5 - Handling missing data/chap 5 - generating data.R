##### Setup #####
library(psych)
library(ggpubr)
library(tidyverse)
library(mice)
library(VIM) # For visualization function md.pattern
library(reshape) #For function melt
library(GGally) # For visualization function ggpairs
library(beepr) # For sound function beep()
library(rstudioapi)

### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

options(scipen=100)

##### Functions definition #####

#Generate complete data
complete_data_gen <- function(N = 500) {
  age <- ifelse(runif(N,0,1)<=0.6,
                rnorm(N, 25,5),
                rnorm(N, 45,5)) %>% 
    round(0) %>% #Rounding to the nearest integer
    pmax(6) %>% #Deleting ages below 6
    pmin(90) #Deleting ages above 90
  
  open <- rnorm (N,5,2) %>%
    pmax(0) %>% #Deleting values below 0
    pmin(10) #Deleting values above 10
  
  extra <- rnorm (N,5,2) %>%
    pmax(0) %>% #Deleting values below 0
    pmin(10) #Deleting values above 10
  
  neuro <- rnorm (N,5,2) %>%
    pmax(0) %>% #Deleting values below 0
    pmin(10) #Deleting values above 10
  
  #Insurance is an auxiliary variable for neuro
  insurance <- (rnorm (N,10,1) + neuro * rnorm (N,5,0.1)) %>%
    pmax(0) #Deleting values below 0
  
  #Active destination is an auxiliary variable for extra
  active <- (rnorm (N,10,1) + extra * rnorm (N,2,0.05)) %>%
    pmax(0) #Deleting values below 0
  
  gender <- sample(c("M","F"), N, replace = TRUE)
  
  state  <- sample(c("A","B", "C"), N, replace = TRUE)
  
  booking_amount <- (200 - age * rnorm(N,1,1) + (gender == "M") * rnorm(N,10,10) +
                       open * rnorm(N, 5,5) + extra * rnorm(N, 10,10) + neuro * rnorm(N,-5,2) + (state == "A") * rnorm(N,10,10)) %>%
    pmax(0) #Deleting values below 0
  
  complete_data <- tibble(
    age = age,
    open = open,
    extra = extra,
    neuro = neuro,
    gender = factor(gender, levels = c("M","F")),
    state = factor(state, levels = c("A","B", "C")),
    insurance = insurance,
    active = active,
    bkg_amt = booking_amount
  )
  return(complete_data)
}

#Generate available date from complete data
available_data_gen <- function(complete_data, N = 500){
  
  available_data <- complete_data %>%
    #Booking amount is deterministically MAR on age
    mutate(bkg_amt = ifelse(age <= quantile(age, probs = c(0.9)), bkg_amt, NA)) %>%
    #Extraversion is MCAR
    mutate(extra = ifelse(runif(N,0,1)>=0.4, extra, NA)) %>%
    #Neuroticism is MNAR
    mutate(neuro = ifelse(neuro >= median(neuro), neuro, NA))
  
  
  #State is probabilistically MAR on age
  prob_state_missing <- logistic(complete_data$age^2/2500-1)
  summary(prob_state_missing)
  plot(complete_data$age, prob_state_missing)
  available_data <- available_data %>%
    mutate(state = as.character(state)) %>%
    mutate(state = ifelse(runif(N,0,1)<=prob_state_missing,"",state)) %>%
    mutate(state = factor(state, levels = c("A", "B", "C")))
  
  return(available_data)
}

tampa_data_fun <- function(N=1e3){
  #Generating complete data
  data <- tibble(
    ID = seq(1,N,1),
    miss = runif(N,0,1),
    I1 = runif(N,0,10),
    I2 = runif(N,0,10),
    I3 = runif(N,0,10)
  )
  #Generating missingness at the individual level
  thres <- 0.2
  data <- data %>%
    mutate(I1 = ifelse(miss <= thres, NA, I1)) %>%
    mutate(I2 = ifelse(miss <= thres, NA, I2)) %>%
    mutate(I3 = ifelse(miss <= thres, NA, I3)) %>%
    select(-miss)
  return(data)
}

tacoma_data_fun <- function(N=1e3){
  #Generating complete data
  data <- tibble(
    ID = seq(1,N,1),
    I1 = runif(N,0,10),
    I2 = runif(N,0,10),
    I3 = runif(N,0,10)
  )
  #Generating missingness at the variable level
  thres <- 0.2
  data <- data %>%
    mutate(I1 = ifelse(runif(N,0,1) <= thres, NA, I1)) %>%
    mutate(I2 = ifelse(runif(N,0,1) <= thres, NA, I2)) %>%
    mutate(I3 = ifelse(runif(N,0,1) <= thres, NA, I3))
  return(data)
}

##### Creating data sets #####
set.seed(1234)
N <- 2000

complete_data <- complete_data_gen(N = N)

write_csv(complete_data, "chap3-complete_data.csv")

available_data <- available_data_gen(complete_data, N = N)

available_data_supp <- available_data %>% select(insurance,active)
available_data <- available_data %>% select(-insurance,-active)

write_csv(available_data, "chap3-available_data.csv")
write_csv(available_data_supp, "chap3-available_data_supp.csv")

tampa <- tampa_data_fun(N)
write_csv(tampa, "chap3-tampa.csv")

tacoma <- tacoma_data_fun(N)
write_csv(tacoma, "chap3-tacoma.csv")
