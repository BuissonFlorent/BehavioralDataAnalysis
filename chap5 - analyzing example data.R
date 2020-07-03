### Part II Chapter 5 : Building CDs from scratch
## chap5 - analyzing example data.R
##
## This script loads the example data, based on data from Antonio, de Almeida & Nunes, 
## "Hotel booking demand datasets", Data in Brief, 2019.  https://doi.org/10.1016/j.dib.2018.11.126.
## and analyzes it


##### Setup #####
library(rcompanion) # For Cramer V correlation coefficient
library(tidyverse)
library(car) #For vif diagnostic function
library(ggpubr) #For function ggrrange

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

#Reducing the number of values for Country of origin by keeping most frequent countries only
#and aggregating the remaining ones under "Other"
countries <- dat %>% 
  group_by(Country) %>% 
  summarize(pct = n()/nrow(dat))
topcountries <- countries %>% 
  filter(pct >= 0.01)
dat <- dat %>%
  mutate(Country = ifelse(Country %in% topcountries$Country, Country, "Other")) %>%
  mutate(Country = as.factor(Country))


##### Exploratory correlation analysis #####

#Cancellation rate by deposit types
with(dat, table(NRDeposit, IsCanceled))
with(dat, prop.table(table(NRDeposit, IsCanceled), 1))

with(dat, rcompanion::cramerV(NRDeposit, IsCanceled))

### Calculating correlation table for all categorical variables

cat_corr_fun <- function(dat){
  # Renaming variables to shorten them
  dat <- dat %>%
    rename(CustTyp= CustomerType) %>%
    rename(DistCh = DistributionChannel) %>%
    rename(RepGst = IsRepeatedGuest) %>%
    rename(MktSgmt = MarketSegment) %>%
    rename(IsCanc = IsCanceled) %>%
    rename(PrevCan = PreviousCancellations) %>%
    rename(NRDep = NRDeposit)
  
  #Going through all categorical variables
  corr_list <- list()
  n <- 1
  for(varI in colnames(dat)){
    for(varJ in colnames(dat)){
      #Factor to factor correlation
      if(is.factor(dat[[varI]]) & is.factor(dat[[varJ]]) & varI != varJ){
        corr = cramerV(dat[[varI]], dat[[varJ]])
        corr_list[[n]] <- data.frame(
          varI = as.character(varI),
          varJ = as.character(varJ),
          corr = corr)
        n <- n + 1
      }
    }
  }
  corr_df = bind_rows(corr_list) %>% 
    spread(varJ, corr)
  return(corr_df)
}
corr_df <- cat_corr_fun(dat)
corr_df

#Removing quarter variable as it is uncorrelated with others
dat <- dat %>%
  select(-Quarter)

### Numerical variables

## Looking at time trends
time_plot_fun <- function(){
  dat_time <- dat %>%
    group_by(Year) %>%
    summarise(Cancel_rate = mean(IsCanceled==1),
              NRDeposit_rate = mean(NRDeposit==1),
              Avg_child_bab = mean(Children),
              Avg_ADR = mean(ADR)
    ) 
  time_p1 <- ggplot(dat_time, aes(x=Year, y=Avg_ADR)) + 
    geom_line() + geom_point() + theme(axis.text.x=element_blank()) + 
    ylab("Average ADR") + ylim(c(90,120))
  time_p2 <- ggplot(dat_time, aes(x=Year, y=Avg_child_bab)) + 
    geom_line() + geom_point() + theme(axis.text.x=element_blank()) + 
    ylab("Average number of children") + ylim(c(0.1,0.2))
  time_p3 <- ggplot(dat_time, aes(x=Year, y=NRDeposit_rate)) + 
    geom_line() + geom_point() + theme(axis.text.x=element_blank()) + 
    ylab("% of non-refundable deposits") + ylim(c(0.01,0.02))
  time_p4 <- ggplot(dat_time, aes(x=Year, y=Cancel_rate)) + 
    geom_line() + geom_point() + theme(axis.text.x=element_blank()) + 
    ylab("Cancellation rate") + ylim(c(0.2,0.4))
  
  ggarrange(time_p1, time_p2, time_p3,time_p4, ncol = 4)
}
time_plot_fun()

#NRDeposit
summary(glm(NRDeposit~Children+Year, 
            data=dat, family = binomial(link = "logit")))
summary(glm(NRDeposit~ADR+Year, 
            data=dat, family = binomial(link = "logit")))

#IsCanceled
summary(glm(IsCanceled~Children+Year, 
            data=dat, family = binomial(link = "logit")))
summary(glm(IsCanceled~ADR+Year, 
            data=dat, family = binomial(link = "logit")))

#Relationship between ADR and number of children
with(dat,boxplot(ADR~Children, xlab="Number of children"))

#Filtering out large numbers of children
dat <- dat %>%
  filter(Children < 4)


##### Regression analysis #####

#Single explanatory variable
mod1 <- glm(IsCanceled ~ NRDeposit, 
            data = dat, family = binomial(link = "logit"))
summary(mod1)

#Complete model
mod_all <- glm(IsCanceled ~ ., 
                   data = dat, family = binomial(link = "logit"))
summary(mod_all)
