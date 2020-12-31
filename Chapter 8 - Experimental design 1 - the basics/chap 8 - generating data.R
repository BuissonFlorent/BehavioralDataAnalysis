##### Setup #####
library(Rlab)
library(tidyverse)

setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Part III Experimental design/Chapter 8 - online streaming exp")

set.seed(1234)
options(scipen=10)


#Generating the data
Nperiods <- 36 #Total number of periods (months), including experiment
N <- 10000 # Average number of customer interaction per period

data_gen <- function(Nperiods, N){
  
  #Simulating the monthly means for the explanatory variables
  time_gender <- rnorm(Nperiods, 0.5, 0.1)
  time_age <- round(rnorm(Nperiods, 40, 2))
  time_interest <- round(rnorm(Nperiods, 10, 1))
  time_N <- round(rnorm(Nperiods, N,100))
  dat_list <- list()
  
  for(i in 1:Nperiods){
    #simulating the values of the explanatory variables
    period <- i
    month <- period %% 12 + 1
    seasonality <- (1 + sin(i * 3.1415 * 2 / 12))/2
    gender_prob <- runif(time_N[i],0,1)
    gender <- ifelse(gender_prob >= time_gender[i],"male","female")
    age <- round(rnorm(time_N[i],time_age[i],5))
    interest <- round(rnorm(time_N[i],time_interest[i],1))
    #Simulating the experimental intervention
    if(i>=Nperiods-3){
      oneclick <- rbern(time_N[i], 0.5)
    } else {
      oneclick <- rep(0, time_N[i])
    }
    
    #simulating the values of the booking probability
    intercept <- 0.3
    beta_p <- -0.01
    beta_s <- 1.0
    beta_g <- 0.3
    beta_a <- -0.5
    beta_i <- 1.5
    beta_1 <- 0.21
    linpred <- intercept + beta_p * period + beta_a * age + beta_g * (gender == "male") + 
      beta_s * seasonality + beta_i * interest + beta_1 * oneclick 
    booked_prob <- exp(linpred)/(1+exp(linpred))
    booked <- rbinom(time_N[i],1,booked_prob)

    dat_list[[i]] <- data.frame(
      age = age,
      gender = gender,
      period = period, 
      seasonality = seasonality,
      month = month,
      booked = booked,
      oneclick = oneclick
    )
  }
  
  dat <- bind_rows(dat_list)
  return(dat)
}

set.seed(1234)
dat <- data_gen(Nperiods, N)

#Splitting the dataset between historical and experimental data
hist_data <- dat %>% 
  select(-oneclick) %>%
  filter(period < 33)
exp_data <- dat %>%
  filter(period >= 33)

write_csv(hist_data, "chap8-historical_data.csv")
write_csv(exp_data, "chap8-experimental_data.csv")

##### Pre/post trends #####

# par(mfrow=c(1,2))
# 
# ts1 <- rnorm(12, 0.4, 0.02)
# ts1[12] <- 0.6
# ts2 <- rnorm(12, 0.4, 0.2)
# ts2[12] <- 0.6
# plot(ts1, ylim=c(0,1), type="line", lwd=5, col="blue")
# points(ts1, lwd=5)
# plot(ts2, ylim=c(0,1), type="line", lwd=5, col="red")
# points(ts2)
# 
# par(new = F)
