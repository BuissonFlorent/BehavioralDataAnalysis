### Part III chapter 10: Experimental design 3 - offline population-based experiment
## chap10 - exercises solutions.R
##
## This script contains some proposed solutions for the exercises in chapter 10
## The example data used in exercise 1 is based on data from Antonio, de Almeida & Nunes, 
## "Hotel booking demand datasets", Data in Brief, 2019.  https://doi.org/10.1016/j.dib.2018.11.126.

library(tidyverse)
library(lme4)

###### Exercise 1 #####

setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Part II Analyzing observational data/Chapter 5 - Building CDs from scratch")

set.seed(1234)
options(scipen=10)

#Loading the data
dat <- read_csv("chap5-hotel_booking_case_study.csv", 
                col_types = cols(
                  NRDeposit = col_factor(),
                  IsCanceled = col_factor(),
                  DistributionChannel = col_factor(),
                  MarketSegment = col_factor(),
                  CustomerType = col_factor(),
                  Children = col_double(),
                  ADR = col_double(),
                  PreviousCancellations = col_factor(),
                  IsRepeatedGuest = col_factor(),
                  Country = col_character(),
                  Quarter = col_factor(),
                  Year = col_double()))

#Fitting a traditional logistic model
mod_all <- glm(IsCanceled ~ NRDeposit + Country, 
               data = dat, family = binomial(link = "logit"))
summary(mod_all)

# Fitting the logistic hierarchical model 

mod<- glmer(IsCanceled ~ NRDeposit + (1 | Country), 
            data = dat, family = binomial)
summary(mod)