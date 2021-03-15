# Common libraries
suppressMessages(suppressWarnings(library(tidyverse)))
library(boot) #Required for Bootstrap simulations
library(rstudioapi) #To load data from local folder
library(ggpubr) #To generate multi-plots

# Chapter-specific libraries
library(blockTools) # For function block()
library(caret) # For one-hot encoding function dummyVars()
library(scales) # For function rescale()

# Libraries for high-performance Bootstrap
library(Rfast)
library(doParallel)

### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

set.seed(1234)
options(scipen=10)

# Loading the data
hist_data <- read_csv("chap8-historical_data.csv")
exp_data <- read_csv("chap8-experimental_data.csv")

#Restating tier as a factor variable
hist_data <- hist_data %>%
  mutate(tier = factor(tier, levels = c(3,2,1))) %>%
  mutate(ID = as.character(ID))
exp_data <- exp_data %>%
  mutate(tier = factor(tier, levels = c(3,2,1))) %>%
  mutate(ID = as.character(ID))

# Function for assignment completely at random with 3 experimental groups 
no_strat_assgnt_fun <- function(dat, Nexp){
  K <- 3  
  dat <- dat %>%
    distinct(ID) %>%
    slice_sample(n=Nexp) %>%
    mutate(assgnt = runif(Nexp,0,1)) %>%
    mutate(group = case_when(
      assgnt <= 1/K ~ "ctrl",
      assgnt > 1/K & assgnt <= 2/K ~ "treat1",
      assgnt > 2/K & assgnt <= 1 ~ "treat2")) %>%
    mutate(group = as.factor(group)) %>%
    select(-assgnt)
  return(dat)
}
#no_strat_assgnt <- no_strat_assgnt_fun(hist_data, Nexp = 4998)

# Extension of the previous function for any number K
no_strat_assgnt_fun <- function(dat, Nexp, K){
  dat <- dat %>%
    distinct(ID) %>%
    slice_sample(n=Nexp) %>%
    mutate(assgnt = runif(Nexp,0,1)) %>%
    mutate(group = -1) # initializing the “group” variable
  for(i in seq(1,K)){
    dat$group = ifelse(dat$assgnt >= (i-1)/K & dat$assgnt < i/K,i-1,dat$group)} 
  dat <- dat %>%
    mutate(group = as.factor(group)) %>%
    select(-assgnt)
  return(dat)
}
#no_strat_assgnt <- no_strat_assgnt_fun(hist_data, Nexp = 5000, K = 4)

# Function to prep the data for stratified randomization
strat_prep_fun <- function(dat){
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
    #Selecting categorical variables
    select_if(is.factor) 
  num_vars <- dat %>%
    #Selecting numeric variables
    select_if(function(x) is.numeric(x)|is.integer(x)) 
  
  #One-hot encoding categorical variables
  cat_vars_out <- data.frame(predict(dummyVars(" ~.", data=cat_vars), 
                                     newdata = cat_vars))
  
  #Normalizing numeric variables
  num_vars_out <- num_vars %>%
    mutate_all(rescale)
  
  #Putting the variables back together
  dat_out <- cbind(ID, num_vars_out, cat_vars_out)  %>%
    mutate(ID = as.character(ID)) %>%
    mutate_if(is.numeric, function(x) round(x, 4)) #Rounding for readability
  
  return(dat_out)
}
prepped_data <- strat_prep_fun(hist_data)
head(prepped_data,5)

#Using the block function directly
block_stratified_data <- block(prepped_data, id.vars = c("ID"), n.tr = 3,
                               algorithm = "naiveGreedy", distance = "euclidean")

#Using a wrapper function for readable output
block_wrapper_fun <- function(dat){
  
  prepped_data <- strat_prep_fun(dat)
  
  #Getting stratified assignment
  assgt <- prepped_data %>%
    block(id.vars = c("ID"), n.tr = 3, 
          algorithm = "naiveGreedy", distance = "euclidean") %>%
    assignment() 
  assgt <- assgt$assg$`1` 
  assgt <- assgt %>%
    select(-'Max Distance')
  colnames(assgt) <- c("ctrl", "treat1","treat2")
  assgt_long <- gather(assgt,group, ID, 'ctrl':'treat2') %>%
    mutate(group = as.factor(group))
  
  dat_final <- full_join(dat, assgt_long, by="ID")
  
  return(dat_final)
}
stratified_data <- block_wrapper_fun(hist_data)
head(stratified_data,3)

# Metric function for free cleaning (treatment 1)
treat1_metric_fun <- function(dat){
  lin_model <- lm(BPday~sq_ft+tier+avg_review+group, data = dat)
  summ <- summary(lin_model)
  coeff <- summ$coefficients['grouptreat1', 'Estimate']
  return(coeff)
}

# Metric function for minimum booking duration (treatment 2)
treat2_metric_fun <- function(dat){
  lin_model <- lm(BPday~sq_ft+tier+avg_review+group, data = dat)
  summ <- summary(lin_model)
  coeff <- summ$coefficients['grouptreat2', 'Estimate']
  return(coeff)
}

# Bootstrap CI function
boot_CI_fun <- function(dat, metric_fun, B=20, conf.level=0.9){
  
  boot_vec <- sapply(1:B, function(x){
    #cat("bootstrap iteration ", x, "\n")
    metric_fun(slice_sample(dat, n = nrow(dat), replace = TRUE))})
  boot_vec <- sort(boot_vec, decreasing = FALSE)
  offset = round(B * (1 - conf.level) / 2)
  CI <- c(boot_vec[offset], boot_vec[B+1-offset])
  return(CI)
}

# decision function
decision_fun <- function(dat, metric_fun, B = 100, conf.level = 0.9){
  boot_CI <- boot_CI_fun(dat, metric_fun, B = B, conf.level = conf.level)
  decision <- ifelse(boot_CI[1]>0,1,0)
  return(decision)
}
decision_fun(stratified_data, treat2_metric_fun)

single_sim_fun <- function(dat, metric_fun, Nexp, eff_size, B = 100, conf.level = 0.9){
  #Filter the data down to a random month
  per <- sample(1:35, size=1)
  dat <- dat %>%
    filter(period == per)
  
  #Prepare the stratified assignment for a random sample of desired size
  stratified_assgnt <- dat %>%
    slice_sample(n=Nexp) %>%
    #Stratified assignment
    block_wrapper_fun() %>%
    #extract the ID and group assignment
    select(ID, group)
  
  sim_data <- dat %>%
    #Apply assignment to full data
    inner_join(stratified_assgnt) %>%
    #Add target effect size
    mutate(BPday = ifelse(group == 'treat2', BPday + eff_size, BPday))
  
  #Calculate the decision (we want it to be 1)
  decision <- decision_fun(sim_data, metric_fun, B = B, conf.level = conf.level)
  return(decision)
}
sim_decision <- single_sim_fun(hist_data, treat2_metric_fun, Nexp=99, eff_size=2)

#Standard function for simulations at scale
power_sim_fun <- function(dat, metric_fun, Nexp, eff_size, Nsim, B = 100, conf.level = 0.9){
  power_list <- vector(mode = "list", length = Nsim)
  for(i in 1:Nsim){
    cat("simulation loop number ", i, "\n")
    power_list[[i]] <- single_sim_fun(dat, metric_fun, Nexp, eff_size, B = B, conf.level = conf.level)
  }
  power <- mean(unlist(power_list))
  return(power)
}

#Optimized function for simulations at scale
fun_lst <- c('block_wrapper_fun', 'treat2_metric_fun', 'boot_CI_fun',  'decision_fun', 
             'single_sim_fun', 'strat_prep_fun')
pckg_lst <- c('boot', 'Rfast', 'tidyverse', 'caret', 'blockTools', 'scales')

opt_power_sim_fun <- function(dat, metric_fun, Nexp, eff_size, Nsim, B = 100, conf.level = 0.9){
  registerDoParallel()
  power_list <- foreach(i=1:Nsim, .export=fun_lst, .packages=pckg_lst) %dopar% {
    single_sim_fun(dat, metric_fun, Nexp, eff_size, B = B, conf.level = conf.level)
  }
  power <- mean(unlist(power_list))
  stopImplicitCluster()
  return(power)
}

# Visualizing the power simulation results for a confidence level of 0.90

# Loading the results of the simulations I have run
pow_sim_res <- read_csv("pow_sim_res.csv")

viz_fun8.3 <- function(){
  ggplot(pow_sim_res, aes(x = sample_size, y = power)) + 
    scale_x_continuous(limits = c(0, 6000), breaks=seq(0,6000,500)) + 
    scale_y_continuous(limits = c(0, 1), breaks=seq(0,1,0.1)) +
    geom_hline(yintercept = 0.9, col = 'red') +
    xlab("sample size") + ylab("power") + labs(size = "number of\nsimulations") +
    geom_point(aes(size=Nsim), alpha = 0.5) + 
    geom_label(aes(label=sim.id), nudge_x = 100, nudge_y = -0.05)
}
viz_fun8.3()

### Visualizing the power curve for various effect sizes at a sample size of 1500 and a confidence level of 0.90
# Loading the results of the simulations I have run
es_sim_res <- read_csv("es_sim_res.csv")

viz_fun8.4 <- function(){
  ggplot(es_sim_res, aes(x = ES, y = power90)) + geom_point() +
    geom_line(data=(es_sim_res %>% filter(ES != 0)), aes(x = ES, y = power90), inherit.aes = FALSE) + 
    geom_hline(yintercept = 0.9, col = 'red') +
    xlab("effect size") + 
    geom_label(x=0.025, y=0.055, label="significance")
}
viz_fun8.4()

### Figure 8-5. Comparison of power curves for confidence levels 0.90 (solid line),  0.80 (dashed line) and 0.60 (dotted line)
viz_fun8.5 <- function(){
  ggplot(es_sim_res, aes(x = ES, y = power90)) + geom_point() +
    geom_line(aes(x = ES, y = power90), inherit.aes = FALSE) + 
    geom_hline(yintercept = 0.9, col = 'red') +
    xlab("effect size") + 
    #Values for confidence level 0.80
    geom_point(aes(x = ES, y = power80)) +
    geom_line(aes(x = ES, y = power80), inherit.aes = FALSE,
              linetype = 'longdash') +
    #Values for confidence level 0.60
    geom_point(aes(x = ES, y = power60)) +
    geom_line(aes(x = ES, y = power60), inherit.aes = FALSE,
              linetype = 'dashed') +
    #Values for confidence level 0.40
    geom_point(aes(x = ES, y = power40)) +
    geom_line(aes(x = ES, y = power40), inherit.aes = FALSE,
              linetype = 'dotted') +
    geom_label(x=0.025, y=0.2, label="significance")
}
viz_fun8.5()

#Linear regression
exp_data_reg <- exp_data %>%
  mutate(BPday = BPday - ifelse(group=="treat2" & compliant, 10,0))
lin_model <- lm(BPday~sq_ft+tier+avg_review+group, data = exp_data_reg)
summary(lin_model)
boot_CI_fun(exp_data_reg, treat1_metric_fun)
boot_CI_fun(exp_data_reg, treat2_metric_fun)

#Calculating the CACE
exp_data_reg %>%
  group_by(group) %>%
  summarise(compliance_rate = mean(compliant))

exp_data_reg %>%
  group_by(group, compliant) %>%
  summarise(coeff = mean(BPday))