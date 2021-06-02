##### 2. Data and libraries #####
# Common libraries
suppressMessages(suppressWarnings(library(tidyverse)))
library(rstudioapi)
library(ggpubr)
library(boot)

# Chapter-specific libraries
library(pwr) # For traditional power analysis

# Libraries for high-performance Bootstrap
library(Rfast)
library(doParallel)

### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

set.seed(1234)
options(scipen=10)

#Reading the data
hist_data <- read_csv("chap8-historical_data.csv")
exp_data <- read_csv("chap8-experimental_data.csv")

#Formatting the data
hist_data <- hist_data %>%
  mutate(gender = factor(gender, levels = c("male", "female"))) %>%
  mutate(month = factor(month))
exp_data <- exp_data %>%
  mutate(gender = factor(gender, levels = c("male", "female"))) %>%
  mutate(oneclick = factor(oneclick)) %>%
  mutate(month = factor(month, levels = levels(hist_data$month)))

##### 3. Determining random assignment and sample size/power #####

#### 3.1 Random assignment ####

# Basic random assignment
K <- 2
assgnt <- runif(1,0,1)
group <- ifelse(assgnt <= 1/K, "control", "treatment")

#### 3.2 Sample size and experiment power ####

effect_size <- ES.h(0.1925,0.1825)
pwr.2p.test(h = effect_size, n = NULL, sig.level = 0.05, power = 0.8, alternative = "greater")

### Null experimental dataset
exp_null_data <- hist_data %>%
  slice_sample(n=20000) %>%
  mutate(oneclick = ifelse(runif(20000)>0.5,1,0)) %>%
  mutate(oneclick = factor(oneclick, levels=c(0,1)))

### Metric function
summary(glm(booked ~ oneclick + age + gender, 
            data = exp_null_data, family = binomial(link = "logit")))

log_reg_fun <- function(dat){
  
  #Running logistic regression
  log_mod_exp <- glm(booked ~ oneclick + age + gender, 
                     data = dat, family = binomial(link = "logit"))
  
  summ <- summary(log_mod_exp)
  metric <- summ$coefficients['oneclick1', 'Estimate']
  
  return(metric)
}

### Bootstrap CI function
boot_CI_fun <- function(dat, metric_fun, B = 100, conf.level = 0.9){
  #Setting the number of bootstrap samples
  boot_metric_fun <- function(dat, J){
    boot_dat <- dat[J,]
    return(metric_fun(boot_dat))
  }
  boot.out <- boot(data=dat, statistic=boot_metric_fun, R=B)
  confint <- boot.ci(boot.out, conf = conf.level, type = c('perc'))
  CI <- confint$percent[c(4,5)]
  
  return(CI)
}

### decision function
decision_fun <- function(dat, metric_fun, B = 100, conf.level = 0.9){
  boot_CI <- boot_CI_fun(dat, metric_fun, B = B, conf.level = conf.level)
  decision <- ifelse(boot_CI[1]>0,1,0)
  return(decision)
}

### Function for single simulation

single_sim_fun <- function(dat, metric_fun, Nexp, eff_size, B = 100, 
                           conf.level = 0.9){
  
  #Adding predicted probability of booking
  hist_mod <- glm(booked ~ age + gender + period, 
                  family = binomial(link = "logit"), data = dat)
  sim_data <- dat %>%
    mutate(pred_prob_bkg = hist_mod$fitted.values) %>%
    #Filtering down to desired sample size
    slice_sample(n = Nexp) %>%
    #Random assignment of experimental groups
    mutate(oneclick = ifelse(runif(Nexp,0,1) <= 1/2, 0, 1)) %>%
    mutate(oneclick = factor(oneclick, levels=c(0,1))) %>%
    # Adding effect to treatment group
    mutate(pred_prob_bkg = ifelse(oneclick == 1, 
                                  pred_prob_bkg + eff_size, 
                                  pred_prob_bkg)) %>%
    mutate(booked = ifelse(pred_prob_bkg >= runif(Nexp,0,1),1, 0))

  #Calculate the decision (we want it to be 1)
  decision <- decision_fun(sim_data, metric_fun, B = B, 
                           conf.level = conf.level)
  return(decision)
}
single_sim_fun(hist_data, log_reg_fun, Nexp = 100, eff_size = 0.01, B = 40, 
               conf.level = 0.9)


## power simulation function
power_sim_fun <- function(dat, metric_fun, Nexp, eff_size, Nsim, 
                          B = 100, conf.level = 0.9){
  power_list <- vector(mode = "list", length = Nsim)
  for(i in 1:Nsim){
    power_list[[i]] <- single_sim_fun(dat, metric_fun, Nexp, eff_size, 
                                      B = B, conf.level = conf.level)

  }
  power <- mean(unlist(power_list))
  return(power)
}

set.seed(1234)
power_sim_fun(dat=hist_data, metric_fun = log_reg_fun, Nexp = 4e4, 
              eff_size=0.01, Nsim=10)

### Figure 8-3 Power simulations for various sample sizes

viz_fun8.3 <- function(){
  power_res <- tibble(
    N = c(4e4, 3e4, 5e4, 35e3, 45e3),
    Nsim = c(20, 100, 100, 200, 200),
    power = c(0.9, 0.8, 0.98, 0.875, 0.93)
  )
  power_res <- power_res %>%
    mutate(Nsim = factor(Nsim))
  
  ggplot(power_res, aes(x=N, y=power)) + 
    geom_point(aes(shape=Nsim, size = 2)) + 
    scale_shape_manual(values=c(4, 20, 15)) +
    geom_line(linetype = "dashed", col='blue') + 
    guides(size=FALSE)
}
viz_fun_8.3()

### Figure 8-4. Power simulations for various effect sizes at N = 40,000, 
### with 200 simulations per effect size, dashed line at power = 0.9

# WARNING! THIS VISUALIZATION USES THE OPTIMIZED FUNCTIONS DEFINED IN THE
# APPENDIX

viz_fun8.4 <- function(){
  es_lst <- seq(from=0.005, to=0.02, by=0.001)
  es_res <- vector(mode='numeric', length=length(es_lst))
  for(i in 1:length(es_lst)){
    es_res[i] <- opt_power_fun(hist_mat, es=es_lst[i], N=4e4, Nsim=400)
  }
  beep()
  
  #Calculating statistical significance of our analysis
  stat.sig <- opt_power_fun(hist_mat, es=0, N=4e4, Nsim=200)
  beep()
  
  power_ss_res <- tibble(
    effect_size = es_lst,
    power = es_res
  )
  
  power_ss_res
  
  ggplot(power_ss_res, aes(x=effect_size, y=power)) + 
    geom_point() + ylim(c(0,1)) +
    geom_line(col='blue') + 
    geom_hline(yintercept = 0.9, col= 'red', linetype = "dashed")
}

##### 4. Analyzing and interpreting experimental results #####

### Booking probability
log_mod_exp <- glm(booked ~ oneclick + age + gender, 
                   data = exp_data, family = binomial(link = "logit"))
summary(log_mod_exp)

### Calculating Bootstrap CI
exp_boot_CI <- boot_CI_fun(exp_data, metric_fun = log_reg_fun)

### Calculating average difference in probabilities
diff_prob_fun <- function(dat, reg_model = log_mod_exp){

  no_button <- dat %>% 
    mutate(oneclick = 0) %>% 
    mutate(oneclick = factor(oneclick, levels=c(0, 1))) %>%
    select(age, gender, oneclick)
  button <- dat %>% 
    mutate(oneclick = 1) %>% 
    mutate(oneclick = factor(oneclick, levels=c(0, 1))) %>%
    select(age, gender, oneclick)
  #Adding the predictions of the model 
  no_button <- no_button %>%
    mutate(pred_mod = predict(object=reg_model, newdata = no_button, 
                              type="response"))
  button <- button %>%
    mutate(pred_mod = predict(object=reg_model, newdata = button, 
                              type="response"))
  #Calculating average difference in probabilities
  diff <- button$pred_mod - no_button$pred_mod
  return(mean(diff))
}
diff_prob_fun(exp_data, reg_model = log_mod_exp)

### Calculating Bootstrap interval for this difference
diff_CI <- boot_CI_fun(exp_data, diff_prob_fun)

#### Appendix: Optimized R code ####

library(Rfast)
library(doParallel)

### Setting up the data

#Add predicted probability of booking to historical data
hist_mod <- glm(booked ~ age + gender + period, family = binomial(link = "logit"), data = hist_data)
hist_data <- hist_data %>%
  mutate(pred_prob_bkg = hist_mod$fitted.values)

### Data generating function
hist_mat <- hist_data %>%
  mutate(genderF = as.numeric(gender)-1) %>%
  select(booked, age, genderF, pred_prob_bkg) %>%
  relocate(booked) %>%
  data.matrix()

### Optimized data generating function
opt_sim_data_gen_fun <- function(mat, es, N){
  
  I <- sample(1:nrow(mat), N)
  sim_mat <- mat[I,]
  sim_mat <- cbind(sim_mat, oneclick=ifelse(runif(N) > 0.5,1,0))
  sim_mat[,'pred_prob_bkg'] <- ifelse(sim_mat[,'oneclick'] == 1, sim_mat[,'pred_prob_bkg'] + es, sim_mat[,'pred_prob_bkg'])
  sim_mat[,'booked'] <- ifelse(sim_mat[,'pred_prob_bkg']>= runif(N), 1, 0)
  sim_mat <- sim_mat[,colnames(sim_mat) != 'pred_prob_bkg']
  return(sim_mat)
}
#sim_mat <- opt_sim_data_gen_fun(mat=hist_mat, es=0.01, N=4e4)
#sim_mat[1:10,]

### Optimized metric function
opt_metric_fun <- function(mat){
  
  #Extracting X and y from data matrix
  y <- mat[,1]
  X <- mat[,2:ncol(mat)]
  
  #Running logistic regression with Rfast
  log_mod_exp <- glm_logistic(X, y)
  metric <- log_mod_exp$be[4]
  
  return(metric)
}
#opt_metric_fun(sim_mat)

### Optimized Bootstrap CI function
opt_boot_CI_fun <- function(mat, opt_metric_fun){
  #Setting the number of bootstrap samples
  B <- 100
  
  boot_metric_fun <- function(mat, J){
    boot_mat <- mat[J,]
    return(opt_metric_fun(boot_mat))
  }
  boot.out <- boot(data=mat, statistic=boot_metric_fun, R=B)
  confint <- boot.ci(boot.out, conf = 0.90, type = c('perc'))
  CI <- confint$percent[c(4,5)]
  
  return(CI)
}
#opt_boot_CI_fun(sim_mat, opt_metric_fun)

### Optimized decision function
opt_decision_fun <- function(mat){
  boot_CI <- opt_boot_CI_fun(mat, opt_metric_fun)
  decision <- ifelse(boot_CI[1]>0,1,0)
  return(decision)
}
#opt_decision_fun(sim_mat)

### Optimized power simulation function
fun_lst <- c('opt_sim_data_gen_fun', 'opt_metric_fun', 'opt_boot_CI_fun',  'opt_decision_fun')
pckg_lst <- c('boot', 'Rfast')

opt_power_fun <- function(mat, es, N, Nsim){
  registerDoParallel()
  power_list <- foreach(i=1:Nsim, .export=fun_lst, .packages=pckg_lst) %dopar% {
    sim_mat <- opt_sim_data_gen_fun (mat=mat, es=es, N=N)
    opt_decision_fun(sim_mat)
  }
  power <- mean(unlist(power_list))
  stopImplicitCluster()
  return(power)
}