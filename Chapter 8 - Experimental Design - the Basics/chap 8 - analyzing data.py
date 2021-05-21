# -*- coding: utf-8 -*-
"""
Created on Sun May 10 11:44:30 2020

@author: Florent
"""

##### 2. Data and libraries #####

import os

import pandas as pd
import numpy as np
from math import asin, sqrt
import statsmodels.formula.api as smf
import statsmodels.stats.power as ssp


#Libraries required to parallelize simulations
from joblib import Parallel, delayed
import psutil


os.chdir('C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\Behavioral data science book\\R scripts\\Part III Experimental design\\Chapter 8 - the basics')

### Loading the data

hist_data_df = pd.read_csv('chap8-historical_data.csv')
exp_data_df = pd.read_csv('chap8-experimental_data.csv')

##### 3. Determining random assignment and sample size/power #####

#### 3.1 Random assignment ####

# Basic random assignment
K = 2
assgnt = np.random.uniform(0,1,1)
group = "control" if assgnt <= 1/K else "treatment"

#### 3.2 Sample size and experiment power ####

effect_size = ssprop.proportion_effectsize(0.194, 0.184)
ssp.tt_ind_solve_power(effect_size = effect_size, 
                       alpha = 0.05, 
                       nobs1 = None, 
                       alternative = 'larger', 
                       power=0.8)

### Metric function
def log_reg_fun(dat_df):
    
    model = smf.logit('booked ~ oneclick + age + gender', data = dat_df)
    res = model.fit(disp=0)
    coeff = res.params['oneclick']
    return coeff

### Bootstrap CI function
def boot_CI_fun(dat_df, metric_fun, B = 100, conf_level = 0.9):
  #Setting sample size
  N = len(dat_df)
  conf_level = conf_level
  coeffs = []
  
  for i in range(B):
      sim_data_df = dat_df.sample(n=N, replace = True)
      coeff = metric_fun(sim_data_df)
      coeffs.append(coeff)
  
  coeffs.sort()
  start_idx = round(B * (1 - conf_level) / 2)
  end_idx = - round(B * (1 - conf_level) / 2)
  
  confint = [coeffs[start_idx], coeffs[end_idx]]  
  
  return(confint)

### decision function
def decision_fun(dat_df, metric_fun, B = 100, conf_level = 0.9):
    boot_CI = boot_CI_fun(dat_df, metric_fun, B = B, conf_level = conf_level)
    decision = 1 if boot_CI[0] > 0  else 0
    return decision
decision_fun(exp_data_df, log_reg_fun)  

### Function for single simulation
def single_sim_fun(Nexp, dat_df = hist_data_df, metric_fun = log_reg_fun, 
                   eff_size = 0.01, B = 100, conf_level = 0.9):
    
    #Adding predicted probability of booking
    hist_model = smf.logit('booked ~ age + gender + period', data = dat_df)
    res = hist_model.fit(disp=0)
    sim_data_df = dat_df.copy()
    sim_data_df['pred_prob_bkg'] = res.predict()
    #Filtering down to desired sample size
    sim_data_df = sim_data_df.sample(Nexp)
    #Random assignment of experimental groups
    sim_data_df['oneclick'] = np.where(np.random.uniform(size=Nexp) <= 0.5, 0, 1)
    # Adding effect to treatment group
    sim_data_df['pred_prob_bkg'] = np.where(sim_data_df.oneclick == 1, 
                                            sim_data_df.pred_prob_bkg + eff_size, 
                                            sim_data_df.pred_prob_bkg)
    sim_data_df['booked'] = np.where(sim_data_df.pred_prob_bkg >= \
                                     np.random.uniform(size=Nexp), 1, 0)
    
    #Calculate the decision (we want it to be 1)
    decision = decision_fun(sim_data_df, metric_fun = metric_fun, B = B, 
                            conf_level = conf_level)
     
    return decision
single_sim_fun(Nexp = 1000)  
   
 
### power simulation function
def power_sim_fun(dat_df, metric_fun, Nexp, eff_size, Nsim, B = 100, 
                  conf_level = 0.9):
    power_lst = []
    for i in range(Nsim):
        print("starting simulation number", i, "\n")
        power_lst.append(single_sim_fun(Nexp = Nexp, dat_df = dat_df, 
                                        metric_fun = metric_fun, 
                                        eff_size = eff_size, B = B, 
                                        conf_level = conf_level))
    power = np.mean(power_lst)
    return(power)

power_sim_fun(dat_df=hist_data_df, metric_fun = log_reg_fun, Nexp = int(4e4), 
              eff_size=0.01, Nsim=20)

#Alternative parallelized function for higher speed
def opt_power_sim_fun(dat_df, metric_fun, Nexp, eff_size, Nsim, B = 100, conf_level = 0.9):
    #Parallelized version with joblib
    n_cpu = psutil.cpu_count() #Counting number of cores on machine
    counter = [Nexp] * Nsim
    res_parallel = Parallel(n_jobs = n_cpu)(delayed(single_sim_fun)(Nexp) for Nexp in counter)
    pwr = np.mean(res_parallel)
    return(pwr)
opt_power_sim_fun(dat_df=hist_data_df, metric_fun = log_reg_fun, Nexp = int(1e3), eff_size=0.01, Nsim=10)


##### Analyzing the results of the experiment #####

### Logistic regression
log_mod_exp = smf.logit('booked ~ age + gender + oneclick', data = exp_data_df)
res = log_mod_exp.fit()
res.summary()

### Calculating average difference in probabilities

### Calculating average difference in probabilities
def diff_prob_fun(dat_df, reg_model = log_mod_exp):
    
    #Creating new copies of data
    no_button_df = exp_data_df.loc[:, 'age':'gender']
    no_button_df.loc[:, 'oneclick'] = 0
    button_df = exp_data_df.loc[:,'age':'gender']
    button_df.loc[:, 'oneclick'] = 1
    
    #Adding the predictions of the model 
    no_button_df.loc[:, 'pred_bkg_rate'] = res.predict(no_button_df)
    button_df.loc[:, 'pred_bkg_rate'] = res.predict(button_df)
    
    diff = button_df.loc[:,'pred_bkg_rate'] - no_button_df.loc[:,'pred_bkg_rate']
    return diff.mean()
    
diff_prob_fun(exp_data_df, reg_model = log_mod_exp)








