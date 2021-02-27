#################################
##### This script generates the simulated data used in chapter 2, 
##### Behaviors, Causality and Prediction
#################################

##### Setup #####
set.seed(1235)
options(scipen=10)

library(tidyverse)

setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/transfered to Github")


##### Generating the stand level data for the first example #####

#Number of data points per month
N = 200

stand_data_gen_fct <- function(N){
  #Entering min and max monthly temperatures
  temp_min <- c(-3.3, 2.4, 14.3, 27, 38.4, 48.6, 56.8, 56.1, 43.1, 31.1, 19.9, 
                2.2)
  temp_max <- c(52.3, 57.2, 73, 82.2, 88.2, 94.1, 96.5, 94.1, 90.5, 82.2, 68.7, 
                55.7)
  
  
  
  #Matrices of temperature data
  temps <- matrix(0, nrow = N, ncol = 12)
  for (i in 1:12){
    temps[,i] <- runif(N, min=temp_min[i], max=temp_max[i])
  }
  temps2 <- matrix(0, nrow = N, ncol = 12)
  for (i in 1:12){
    temps2[,i] <- runif(N, min=temp_min[i], max=temp_max[i])
  }
  
  #Matrix of dummy for summer months
  summer_months <- matrix(0, nrow = N, ncol = 12)
  for (i in 1:12){
    summer_months[,i] <- ifelse(i %in% c(7,8), 1, 0)
  }
  
  #Matrices of coefficients for regression (ice-cream sales)
  mult_temp1 <- matrix(rnorm(12*N,1000,100), nrow = N, ncol = 12)
  mult_summer_months1 <- matrix(rnorm(12*N,20000,100), nrow = N, ncol = 12)
  noise1 <- matrix(rnorm(12*N,0,500), nrow = N, ncol = 12)
  
  #Matrices of coefficients for iced-coffee sales
  mult_temp2 <- matrix(rnorm(12*N,1000,1), nrow = N, ncol = 12)
  noise2 <- matrix(rnorm(12*N,0,1), nrow = N, ncol = 12)
  
  #Matrix of icecream sales                 
  icecream_sales <- temps * mult_temp1 + summer_months * mult_summer_months1 + 
    noise1
  
  #Matrix of iced-coffee sales
  iced_coffee_sales <- temps * mult_temp2 + noise2
  
  #Creating vector variables for regression (ice cream)
  icecream_sales_v <- as.vector(icecream_sales)
  temps_v <- as.vector(temps)
  summer_months_v <- as.vector(summer_months)
  
  #Creating vector variables for regression (iced coffee)
  iced_coffee_sales_v <- as.vector(iced_coffee_sales)
  
  dat <- tibble(
    icecream_sales = icecream_sales_v,
    iced_coffee_sales = iced_coffee_sales_v,
    summer_months = summer_months_v,
    temps = temps_v
  )
  
  return(dat)
}

stand_dat <- stand_data_gen_fct(N)

write_csv(stand_dat,"chap2-stand_data.csv")

##### Generating the survey data for the second example #####

#Total number of people surveyed
N=10000

survey_data_gen_fct <- function(N){
  #Creating variables for tastes
  vanilla <- rnorm(N, 10, 3) %>% pmax(0)
  chocolate <- rnorm(N, 15, 3) %>% pmax(0)
  
  #Creating shopped variable
  linpred <- (0.01 * vanilla + 0.02 * chocolate)
  prob <- exp(linpred)/(1 + exp(linpred))
  shopped <- ifelse(prob>0.6,1,0)
  
  dat <- tibble(vanilla, chocolate, shopped)
  
  return(dat)
}

survey_dat <- survey_data_gen_fct(N)

write_csv(survey_dat,"chap2-survey_data.csv")