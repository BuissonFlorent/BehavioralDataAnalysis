#################################
##### This script analyzes the data used in chapter 11, 
##### Introduction to Moderation
#################################

##### Setup #####

#Common libraries
library(tidyverse)
library(ggpubr)

#Chapter-specific libraries
library(Rlab)
library(mltools) #For function one_hot
library(data.table) #For function as.data.table
library(Rfast) #For function lmfit
library(parallel)
library(doParallel)

library(rstudioapi)
### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)


set.seed(1234)
options(scipen=10)

#Reading the data
hist_data <- read_csv("chap11-historical_data.csv")

#Formatting the data
hist_data <- hist_data %>%
  mutate(store_id = factor(store_id)) %>%
  mutate(day = factor(day)) %>%
  mutate(children = factor(children)) %>%
  mutate(play_area = factor(play_area))

##### Section 1: Varieties of Moderation #####

#### Segmentation ####

summary(lm(duration~play_area * children, data=hist_data))

### Figure 11-3. Visual representation of moderation ###
viz_fun_11.3 <- function(dat){
  #Representing visually moderation
  summary_dat0 <- dat %>%
    group_by(children,play_area) %>%
    summarize(avg_duration = mean(duration)) %>%
    data.frame() 
  
  mod_p0 <- ggplot(summary_dat0, aes(x=children, y=avg_duration, 
                                     group=play_area)) + ylim(c(0,65)) + 
    geom_point() + geom_line(aes(lty=play_area)) + ylab("Average visit duration") +
    scale_linetype_manual(values=c("dotted", "solid"))
  mod_p0
}

viz_fun_11.3(hist_data)

#### Non-linearities ####

#Toy dataset for this subsection
nonlin_data <- tibble(
  Emails = seq(from = 1, to = 10, by = 0.5),
  Purchases = log(Emails) + rnorm(19, 0, 0.1),
  Properties = seq(from = 1, to = 10, by = 0.5),
  Customers = Properties^2 + rnorm(19, 0, 2))
   
nonlin_data <- nonlin_data %>%
  mutate(pred_PP_lin = lm(data=nonlin_data, Purchases~Emails)$fitted.values,
         pred_PP_quad = lm(data=nonlin_data, 
                           Purchases~Emails+I(Emails^2))$fitted.values)

### Figure 11-14. Non-linear relationships between variables ###
viz_fun_11.14 <- function(dat){
  p1 <- ggplot(dat, aes(Emails, Purchases)) + geom_point() + xlab("avg. monthly marketing emails") + ylab("avg. monthly purchases") 
  p2 <- ggplot(dat, aes(Properties, Customers)) + geom_point() + xlab("number of properties (1000s)") + ylab("number of customers (1000s)")
  ggarrange(p1, p2, ncol=2, nrow=1)
}
viz_fun_11.14(nonlin_data)

### Figure 11-15. Linear (dashed) and quadratic (solid) lines of best fit ###
viz_fun_11.15 <- function(dat){
  ggplot(dat, aes(Emails, Purchases)) + geom_point(shape=16) + 
    xlab("avg. monthly marketing emails") + 
    ylab("avg. monthly purchases") +
    geom_line(aes(y=pred_PP_lin, col='red'), lty="dashed", show.legend=FALSE) +
    geom_line(aes(y=pred_PP_quad, col='blue'), show.legend=FALSE)
}
viz_fun_11.15(nonlin_data)

# Syntax for self-moderation
summary(lm(Purchases ~ Emails + I(Emails^2), data=nonlin_data))

##### Section 2: How To Apply Moderation #####

#### When to look for moderation ####

#hist_data <- hist_data %>% mutate(age_quart = ntile(age, 4))

#### Multiple Moderators ####

### Figure 11-23. Moderated moderation across different age groups ###
viz_fun_11.23 <- function(dat){
  summary_dat <- dat %>%
    mutate(age_grp = factor(round(age/10)*10, ordered = TRUE, 
                            levels = c("20","30","40","50","60","70","80"))) %>%
    group_by(children,play_area, age_grp) %>%
    summarize(avg_duration = mean(duration)) %>%
    data.frame() 
  
  mod_p1 <- ggplot(summary_dat, aes(x=children, y=avg_duration, group=play_area)) + 
    ylim(c(0,65)) + geom_point() + geom_line(aes(lty=play_area)) + 
    labs(y="Average visit duration") + scale_linetype_manual(values=c("dotted", 
                                                                      "solid")) + 
    facet_grid(.~age_grp)
  mod_p1
}
viz_fun_11.23(hist_data)

#Regression summary
summary(lm(duration~play_area * children * age, data=dat))

#### Validating Moderation With Bootstrap ####

# Function for bootstrap
mod_boot_fun <- function(dat, B, N){
  set.seed(1)
  #One-hot encoding factors in data
  dat <- dat %>%
    dplyr::select(-day,-store_id,-prop_children) %>%
    as.data.table() %>%
    mltools::one_hot(dropUnusedLevels = TRUE) %>% 
    mutate(const = 1) %>%
    #Adding moderation variable 
    mutate(inter = children_1 * play_area_1)
  #Converting the original data to a matrix
  mat <- data.matrix(dat)
  
  #Preallocating memory to result vector
  boot_list <- list()
  
  #Generating the random numbers for the bootstrap
  
  rng <- matrix(data = sample(1:N, size = N*B, replace = TRUE),
                nrow = N, ncol = B)
  
  loop_fun <- function(k){
    if(k %% 10 == 0){
      cat("starting iteration", k, "\n")
    }
    boot_mat <- mat[rng[,k],]
    
    #Coefficients for moderated effect
    X <- boot_mat[,c('play_area_1', 'children_1', 'inter', 'age', 'const')]
    Y <- boot_mat[,'duration']
    
    coeffs <- (Rfast::lmfit(X, Y)$be)
    
    res <- c(
      sample_size = N,
      coeff_0 = as.numeric(coeffs['const',]),
      coeff_p = as.numeric(coeffs['play_area_1',]),
      coeff_c = as.numeric(coeffs['children_1',]),
      coeff_i = as.numeric(coeffs['inter',])
    )
    return(res)
  }

  ## Parallelized bootstrap
  #Detecting the number of cores for parallel processing
  numCores <- detectCores()
  registerDoParallel(numCores)
  
  boot_list <- foreach(i=1:B, .packages = 'Rfast') %dopar% loop_fun(i)
  
  stopImplicitCluster()
  
  boot_summary <- bind_rows(boot_list)
  return(boot_summary)
}

#Running the bootstrap simulation with 1k samples of 10k rows
mod_summ <- mod_boot_fun(hist_data, B = 1e3, N = 1e5)

### Figure 11-25. Distribution of bootstrapped values for interaction coefficient (1k samples of 10k rows) ###
viz_fun_11.25 <- function(dat){
  ggplot(dat, aes(x=coeff_i)) + geom_histogram() + xlim(c(19.5,22.5)) +
    geom_vline(xintercept = 20.985, col='red') + xlab("beta_pc")
}
viz_fun_11.25(mod_summ)

#Running the bootstrap simulation with 1k samples of 200k rows
mod_summ <- mod_boot_fun(hist_data, B = 1e3, N = 2e5)

### Figure 11-26. Distribution of bootstrapped values for interaction coefficient (1k samples of 200k rows) ###
viz_fun_11.26 <- function(dat){
  ggplot(mod_summ, aes(x=coeff_i)) + geom_histogram() + xlim(c(19.5,22.5)) +
    geom_vline(xintercept = 20.985, col='red') + xlab("beta_pc")
}
viz_fun_11.26(mod_summ)

#### Interpreting individual coefficients ####

# Creating smaller sample for readability of figures
set.seed(1)
extract_dat <- hist_data %>% slice_sample(n=1e3) 


### Figure 11-27. Sample of 1,000 data points and regression lines with a play area (full dots, solid line) and without (crosses, dashed line), without moderation term ###
viz_fun_11.27 <- function(dat){
  ggplot(extract_dat, aes(x=age, y=duration, col=play_area)) + 
    geom_point(aes(shape = play_area), alpha = 0.8) +
    scale_shape_manual(values=c(4, 16)) +
    geom_abline(intercept = 25, slope = -0.024, col = 'red', lty='dashed', size = 1.5) + 
    geom_abline(intercept = 25+12.5568, slope = -0.024, col = 'blue')
}
viz_fun_11.27(extract_dat)

### Figure 11-28. Sample of 1,000 data points and regression lines with a play area (full dots, solid line) and without (crosses, dashed line), with moderation term ###
viz_fun_11.28 <- function(dat){
  ggplot(extract_dat, aes(x=age, y=duration, col=play_area)) + 
    geom_point(aes(shape = play_area), alpha = 0.8) +
    scale_shape_manual(values=c(4, 16)) +
    geom_abline(intercept = 23.82, slope = 0, col = 'red', lty='dashed', size = 1.5) + 
    geom_abline(intercept = 23.82+15.852, slope = -0.0659, col = 'blue')
}
viz_fun_11.28(extract_dat)

### Setting meaningful reference points

#Centering age
centered_data <- hist_data %>%
  mutate(age = age - mean(age))
#Resetting default for play_area
centered_data <- hist_data %>%
  mutate(play_area = factor(play_area, levels=c('1','0')))

### Calculating effects at the level of business decisions

business_metric_fun <- function(dat){
  mod_model <- lm(duration~play_area * (children + age), data=dat)
  
  action_dat <- dat %>%
    filter(play_area == 0)
  action_dat <- action_dat %>%
    mutate(pred_dur0 = predict(mod_model, action_dat)) %>%
    mutate(play_area = factor('1', levels=c('0', '1')))
  action_dat <- action_dat %>%
    mutate(pred_dur1 = predict(mod_model, action_dat)) %>%  
    mutate(pred_dur_diff = pred_dur1 - pred_dur0) %>%
    dplyr::group_by(store_id) %>%
    summarise(mean_d = mean(pred_dur_diff), sum_d = sum(pred_dur_diff))
  
  return(action_dat)
}
action_summ_dat <- business_metric_fun(hist_data)
summary(action_summ_dat)

