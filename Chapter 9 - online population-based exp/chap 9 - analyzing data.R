##### Setup #####
library(pwr)
library(Rlab)
library(tidyverse)
library(MBESS)

setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Part III Experimental design/Chapter 9 - online population-based exp")
options(scipen=10)

#Reading the data
hist_data <- read_csv("chap9-historical_data.csv")
exp_data <- read_csv("chap9-experimental_data.csv")

#Restating tier as a factor variable
hist_data <- hist_data %>%
  mutate(tier = factor(tier, levels = c(3,2,1), ordered = TRUE ))
exp_data <- exp_data %>%
  mutate(tier = factor(tier, levels = c(3,2,1), ordered = TRUE ))


##### Demonstration of blocking experimental groups #####
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

blocked_data <- blocking_fct(dat = hist_data, N = N)

### Validating the stratification

#Random assignnment without stratification
K <- 3
blocked_data <- blocked_data %>%
  mutate(assgnt = runif(N,0,1)) %>%
  mutate(group2 = case_when(
    assgnt <= 1/K ~ "ctrl",
    assgnt > 1/K & assgnt <= 2/K ~ "treat1",
    assgnt > 2/K & assgnt <= 1 ~ "treat2")) %>%
  mutate(group2 = as.factor(group2)) %>%
  select(-assgnt)

#Comparison of numeric variables in control group
blocked_data %>%
  group_by(group) %>%
  summarize(count = n(),
            sq_ft = mean(sq_ft),
            tier1 = sum(tier==1),
            tier2 = sum(tier==2),
            tier3 = sum(tier==3),
            avg_review = mean(avg_review),
            BPday = mean(BPday))

blocked_data %>%
  group_by(group2) %>%
  summarize(count = n(),
            sq_ft = mean(sq_ft),
            tier1 = sum(tier==1),
            tier2 = sum(tier==2),
            tier3 = sum(tier==3),
            avg_review = mean(avg_review),
            BPday = mean(BPday))   

##### Simulations for power analysis #####

### Statistical significance analysis of historical data - single run
K <- 3
N <- 2001

#Sampling historical data and generating stratified random group assignment without any effect
set.seed(1234)
sim_data <- blocking_fct(hist_data, N)
sim_data <- sim_data %>%
  rename(group_strat = group)

#Generating unstratified random group assignment without any effect
assgnt <- runif(N,0,1)
group_nostrat <- case_when(
  assgnt <= 1/K ~ "ctrl",
  assgnt > 1/K & assgnt <= 2/K ~ "treat1",
  assgnt > 2/K & assgnt <= 1 ~ "treat2"
)

sim_data <- sim_data %>%
  mutate(group_nostrat = factor(group_nostrat))

#Running linear regression on stratified data
model_strat <- lm(BPday~sq_ft+tier+avg_review+group_strat, 
            data = sim_data)
summary(model_strat)

#Running linear regression on non-stratified data
model_nostrat <- lm(BPday~sq_ft+tier+avg_review+group_nostrat, 
                  data = sim_data)
summary(model_nostrat)

### Statistical power analysis of historical data - single run

#Sampling historical data and generating stratified random group assignment with $2 effect
set.seed(1234)
sim_data <- blocking_fct(hist_data, N)
sim_data <- sim_data %>%
  rename(group_strat = group) %>%
  mutate(BPday_strat = BPday + ifelse(group_strat == "ctrl", 0, 2))

#Generating unstratified random group assignment with $2 effect
assgnt <- runif(N,0,1)
group_nostrat <- case_when(
  assgnt <= 1/K ~ "ctrl",
  assgnt > 1/K & assgnt <= 2/K ~ "treat1",
  assgnt > 2/K & assgnt <= 1 ~ "treat2"
)
sim_data <- sim_data %>%
  mutate(group_nostrat = factor(group_nostrat)) %>%
  mutate(BPday_nostrat = BPday + ifelse(group_nostrat == "ctrl", 0, 2))

#Running linear regression on stratified data
model_strat <- lm(BPday_strat~sq_ft+tier+avg_review+group_strat, 
                  data = sim_data)
summary(model_strat)

#Running linear regression on non-stratified data
model_nostrat <- lm(BPday_nostrat~sq_ft+tier+avg_review+group_nostrat, 
                  data = sim_data)
summary(model_nostrat)

### Statistical significance analysis of historical data - repeated runs with constant sample size
#options(warn=-1)
Nloop <- 20
set.seed(1234)
ptm <- proc.time()
sim_list <- list()
for(i in 1:Nloop){
  if(i%% 10 == 0) cat("Iteration number", i, "\n")
  #Sampling historical data and generating stratified random group assignment
  sim_data <- blocking_fct(hist_data, N)
  sim_data <- sim_data %>%
    rename(group_strat = group)
  
  #Generating unstratified random group assignment without any effect
  assgnt <- runif(N,0,1)
  group_nostrat <- case_when(
    assgnt <= 1/K ~ "ctrl",
    assgnt > 1/K & assgnt <= 2/K ~ "treat1",
    assgnt > 2/K & assgnt <= 1 ~ "treat2"
  )
  
  sim_data <- sim_data %>%
    mutate(group_nostrat = factor(group_nostrat))
  
  #Running linear regression on stratified data
  model_strat <- lm(BPday~sq_ft+tier+avg_review+group_strat, 
                    data = sim_data)
  
  #Running linear regression on non-stratified data
  model_nostrat <- lm(BPday~sq_ft+tier+avg_review+group_nostrat, 
                      data = sim_data)
  sim_list[[i]] <- data.frame(
    strat_coeff1 = model_strat$coefficients['group_strattreat1'],
    strat_pvalue1 = coef(summary(model_strat))[6,4],
    strat_coeff2 = model_strat$coefficients['group_strattreat2'],
    strat_pvalue2 = coef(summary(model_strat))[7,4],
    nostrat_coeff1 = model_nostrat$coefficients['group_nostrattreat1'],
    nostrat_pvalue1 = coef(summary(model_nostrat))[6,4],
    nostrat_coeff2 = model_nostrat$coefficients['group_nostrattreat2'],
    nostrat_pvalue2 = coef(summary(model_nostrat))[7,4]
  )
}
sim_data_summary <- bind_rows(sim_list)
proc.time() - ptm
summary(sim_data_summary)
options(warn=0)

#Determining the empirical stat. sig. thereshold
par(mfrow=c(1,2))
with(sim_data_summary, plot(nostrat_coeff1, nostrat_pvalue1, xlim=c(-3,3), ylim=c(0,1),
                            main = "empirical stat. sig. with non-stratified data", 
                            xlab = "estimated coefficient for treat. 1", 
                            ylab = "estimated p-value"))
abline(h=0.1, col='red')  
with(sim_data_summary, plot(strat_coeff1, strat_pvalue1, xlim=c(-3,3), ylim=c(0,1),
                            main = "empirical stat. sig. with stratified data", 
                            xlab = "estimated coefficient for treat. 1", 
                            ylab = "estimated p-value"))
abline(h=0.1, col='red')
par(new = F)



#Determining statistical significance of regression
sim_data_summary %>%
  summarize(strat_coeff1_sig = sum(strat_pvalue1 < 0.1 & strat_coeff1 > 0)/n(),
            nostrat_coeff1_sig = sum(nostrat_pvalue1 < 0.1 & nostrat_coeff1 > 0)/n())

### Statistical power analysis of historical data - repeated runs with constant sample size

Nloop <- 200
set.seed(1234)
ptm <- proc.time()
sim_list <- list()
for(i in 1:Nloop){
  if(i%% 10 == 0) cat("Iteration number", i, "\n")
  #Sampling historical data and generating stratified random group assignment with a $2 effect
  sim_data <- blocking_fct(hist_data, N)
  sim_data <- sim_data %>%
    rename(group_strat = group) %>%
    mutate(BPday_strat = BPday + ifelse(group_strat == "ctrl", 0, rnorm(N,2,1)))
  
  #Generating unstratified random group assignment with a $2 effect
  assgnt <- runif(N,0,1)
  group_nostrat <- case_when(
    assgnt <= 1/K ~ "ctrl",
    assgnt > 1/K & assgnt <= 2/K ~ "treat1",
    assgnt > 2/K & assgnt <= 1 ~ "treat2"
  )
  
  sim_data <- sim_data %>%
    mutate(group_nostrat = factor(group_nostrat)) %>%
    mutate(BPday_nostrat = BPday + ifelse(group_nostrat == "ctrl", 0,rnorm(N,2,1)))
  
  #Running linear regression on stratified data
  model_strat <- lm(BPday_strat~sq_ft+tier+avg_review+group_strat, 
                    data = sim_data)
  
  #Running linear regression on non-stratified data
  model_nostrat <- lm(BPday_nostrat~sq_ft+tier+avg_review+group_nostrat, 
                      data = sim_data)
  sim_list[[i]] <- data.frame(
    strat_coeff1 = model_strat$coefficients['group_strattreat1'],
    strat_pvalue1 = coef(summary(model_strat))[6,4],
    strat_coeff2 = model_strat$coefficients['group_strattreat2'],
    strat_pvalue2 = coef(summary(model_strat))[7,4],
    nostrat_coeff1 = model_nostrat$coefficients['group_nostrattreat1'],
    nostrat_pvalue1 = coef(summary(model_nostrat))[6,4],
    nostrat_coeff2 = model_nostrat$coefficients['group_nostrattreat2'],
    nostrat_pvalue2 = coef(summary(model_nostrat))[7,4]
  )
}
sim_data_summary <- bind_rows(sim_list)
proc.time() - ptm
summary(sim_data_summary)
  
#Determining the empirical stat. power thereshold
par(mfrow=c(1,2))
with(sim_data_summary, plot(nostrat_coeff1, nostrat_pvalue1, ylim=c(0,1),
                            main = "empirical stat. power with non-stratified data", 
                            xlab = "estimated coefficient for treat. 1", 
                            ylab = "estimated p-value"))
abline(h=0.1, col='red')  
with(sim_data_summary, plot(strat_coeff1, strat_pvalue1, ylim=c(0,1),
                            main = "empirical stat. power with stratified data", 
                            xlab = "estimated coefficient for treat. 1", 
                            ylab = "estimated p-value"))
abline(h=0.1, col='red')
par(new = F)

#Determining statistical power of regression
sim_data_summary %>%
  summarize(strat_coeff1_sig = sum(strat_pvalue1 < 0.1 & strat_coeff1 > 0)/n(),
            nostrat_coeff1_sig = sum(nostrat_pvalue1 < 0.1 & nostrat_coeff1 > 0)/n())

##### Simulations for sample size determination #####

#Simulation loop 
sim_list <- list()
Nreps <- 200
K <- 3
sample_sizes <- seq(from=100, to=2500, by=100) %>%
  rep(Nreps)
Nloop <- length(sample_sizes)

set.seed(1234)
ptm <- proc.time()
for(i in 1:Nloop){
  if(i%% 100 == 0) cat("Iteration number", i, "\n")
  #Sampling historical data and generating stratified random group assignment
  N <- sample_sizes[i] - (sample_sizes[i] %% 3)
  sim_data <- blocking_fct(hist_data, N = N)
  sim_data <- sim_data %>%
    rename(group_strat = group)
  
  #Generating unstratified random group assignment 
  assgnt <- runif(N,0,1)
  group_nostrat <- case_when(
    assgnt <= 1/K ~ "ctrl",
    assgnt > 1/K & assgnt <= 2/K ~ "treat1",
    assgnt > 2/K & assgnt <= 1 ~ "treat2"
  )
  
  sim_data <- sim_data %>%
    mutate(group_nostrat = factor(group_nostrat))
  
  #Running linear regression on stratified data with no effect
  model_strat <- lm(BPday~sq_ft+tier+avg_review+group_strat, 
                    data = sim_data)
  strat_coeff1_no_effect <- model_strat$coefficients['group_strattreat1']
  strat_pvalue1_no_effect <- coef(summary(model_strat))[6,4]
  strat_coeff2_no_effect <- model_strat$coefficients['group_strattreat2']
  strat_pvalue2_no_effect <- coef(summary(model_strat))[7,4]

  #Running linear regression on non-stratified data with no effect
  model_nostrat <- lm(BPday~sq_ft+tier+avg_review+group_nostrat, 
                      data = sim_data)
  nostrat_coeff1_no_effect <- model_nostrat$coefficients['group_nostrattreat1']
  nostrat_pvalue1_no_effect <- coef(summary(model_nostrat))[6,4]
  nostrat_coeff2_no_effect <- model_nostrat$coefficients['group_nostrattreat2']
  nostrat_pvalue2_no_effect <- coef(summary(model_nostrat))[7,4]
  
  #Adding true effect to data
  sim_data <- sim_data %>%
    mutate(BPday_strat = BPday + ifelse(group_strat == "ctrl", 0, rnorm(N,2,1))) %>%
    mutate(BPday_nostrat = BPday + ifelse(group_nostrat == "ctrl", 0,rnorm(N,2,1)))
  
  #Running linear regression on stratified data with $2 effect
  model_strat <- lm(BPday_strat~sq_ft+tier+avg_review+group_strat, 
                    data = sim_data)
  strat_coeff1_2d_effect <- model_strat$coefficients['group_strattreat1']
  strat_pvalue1_2d_effect <- coef(summary(model_strat))[6,4]
  strat_coeff2_2d_effect <- model_strat$coefficients['group_strattreat2']
  strat_pvalue2_2d_effect <- coef(summary(model_strat))[7,4]
  
  #Running linear regression on non-stratified data with $2 effect
  model_nostrat <- lm(BPday_nostrat~sq_ft+tier+avg_review+group_nostrat, 
                      data = sim_data)
  nostrat_coeff1_2d_effect <- model_nostrat$coefficients['group_nostrattreat1']
  nostrat_pvalue1_2d_effect <- coef(summary(model_nostrat))[6,4]
  nostrat_coeff2_2d_effect <- model_nostrat$coefficients['group_nostrattreat2']
  nostrat_pvalue2_2d_effect <- coef(summary(model_nostrat))[7,4]
  
  
  sim_list[[i]] <- data.frame(
    sample_size = N,
    strat_coeff1_no_effect     = strat_coeff1_no_effect,
    strat_pvalue1_no_effect    = strat_pvalue1_no_effect,
    strat_coeff2_no_effect     = strat_coeff2_no_effect,
    strat_pvalue2_no_effect    = strat_pvalue2_no_effect,
    strat_coeff1_2d_effect     = strat_coeff1_2d_effect,
    strat_pvalue1_2d_effect    = strat_pvalue1_2d_effect,
    strat_coeff2_2d_effect     = strat_coeff2_2d_effect,
    strat_pvalue2_2d_effect    = strat_pvalue2_2d_effect,
    nostrat_coeff1_no_effect   = nostrat_coeff1_no_effect,
    nostrat_pvalue1_no_effect  = nostrat_pvalue1_no_effect,
    nostrat_coeff2_no_effect   = nostrat_coeff2_no_effect,
    nostrat_pvalue2_no_effect  = nostrat_pvalue2_no_effect,
    nostrat_coeff1_2d_effect   = nostrat_coeff1_2d_effect,
    nostrat_pvalue1_2d_effect  = nostrat_pvalue1_2d_effect,
    nostrat_coeff2_2d_effect   = nostrat_coeff2_2d_effect,
    nostrat_pvalue2_2d_effect  = nostrat_pvalue2_2d_effect
  )
}
sim_data_summary <- bind_rows(sim_list)
proc.time() - ptm
summary(sim_data_summary)
options(warn=0)

#Determining statistical significance of regression
results0 <- sim_data_summary %>%
  group_by(sample_size) %>%
  summarize(sig_strat   = sum(strat_pvalue1_no_effect < 0.1 & strat_coeff1_no_effect > 0)/n(),
            sig_nostrat = sum(nostrat_pvalue1_no_effect < 0.1 & nostrat_coeff1_no_effect > 0)/n())
results0

#Determining joint statistical significance of regression
results0joint <- sim_data_summary %>%
  group_by(sample_size) %>%
  summarize(sig_strat   = sum((strat_pvalue1_no_effect < 0.1 & strat_coeff1_no_effect > 0)|
                                (strat_pvalue2_no_effect < 0.1 & strat_coeff2_no_effect > 0))/n(),
            sig_nostrat = sum((nostrat_pvalue1_no_effect < 0.1 & nostrat_coeff1_no_effect > 0)|
              (nostrat_pvalue2_no_effect < 0.1 & nostrat_coeff2_no_effect > 0))/n())
results0joint



#Determining statistical power of regression
results1 <- sim_data_summary %>%
  group_by(sample_size) %>%
  summarize(pow_strat   = sum(strat_pvalue1_2d_effect < 0.1 & strat_coeff1_2d_effect > 0)/n(),
            pow_nostrat = sum(nostrat_pvalue1_2d_effect < 0.1 & nostrat_coeff1_2d_effect > 0)/n())
results1  
 
par(mfrow=c(1,2))
with(results1, plot(sample_size, pow_strat, ylim=c(0,1),
                    main = "empirical stat. power with stratified data", 
                    xlab = "sample size", 
                    ylab = "empirical power"))
abline(h=0.9, col='red')
with(results1, plot(sample_size, pow_nostrat, ylim=c(0,1),
                    main = "empirical stat. power with non-stratified data", 
                    xlab = "sample size", 
                    ylab = "empirical power"))
abline(h=0.9, col='red')
par(new = F)

##### Analyzing experimental data #####


#T-test of means for treatment 1
data_test <- exp_data %>%
  filter(group != "treat2") %>%
  select(group, BPday)
t.test(BPday~group, alternative = "less", data=data_test)

#T-test of means for treatment 2
data_test <- exp_data %>%
  filter(group != "treat1") %>%
  mutate(BPday = BPday - ifelse(group=="treat2", 10,0)) %>%
  select(group, BPday)
t.test(BPday~group, alternative = "less", data=data_test)

#Linear regression
data_reg <- exp_data %>%
  mutate(BPday = BPday - ifelse(group=="treat2", 10,0))

lin_model <- lm(BPday~sq_ft+tier+avg_review+group, data = data_reg)
summary(lin_model)

confint(lin_model, 'grouptreat2', level=0.9)

