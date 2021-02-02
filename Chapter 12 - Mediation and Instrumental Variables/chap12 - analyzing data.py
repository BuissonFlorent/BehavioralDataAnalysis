# -*- coding: utf-8 -*-
"""
Created on Sun May 10 11:44:30 2020

@author: Florent
"""

#################################
##### This script analyzes the data used in chapter 12, 
##### Mediation and Instrumental Variables
#################################

##### Setup #####

#Common libraries
import os
import pandas as pd
from statsmodels.formula.api import ols
import numpy as np


#Loading the data from the chapter on moderation, then setting the right folder
os.chdir('C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\Behavioral data science book\\R scripts\\Part IV Advanced tools\\Chapter 11 - Introduction to moderation')
hist_data_df = pd.read_csv('chap11-historical_data.csv')
os.chdir('C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\Behavioral data science book\\R scripts\\Part IV Advanced tools\\Chapter 12 - Mediation and Instrumental Variables')

##### 1. Mediation #####

#### 1.1. Definition and Measurement ####

ols("duration~play_area", data=hist_data_df).fit().summary()
ols("groceries_purchases~play_area", data=hist_data_df).fit().summary()
ols("groceries_purchases~duration", data=hist_data_df).fit().summary()
ols("groceries_purchases~duration+play_area", data=hist_data_df).fit().summary()

### Bootstrap CI for percentage mediated 

def percentage_mediated_fun(dat_df):
    
    total_effect = ols("groceries_purchases~play_area", data=dat_df).fit(disp=0).params['play_area']
    coeff_med1 = ols("duration~play_area", data=dat_df).fit(disp=0).params['play_area']
    coeff_med2 = ols("groceries_purchases~duration", data=dat_df).fit(disp=0).params['duration']
    mediated_effect = coeff_med1 * coeff_med2
    percentage_mediated = mediated_effect / total_effect
    return percentage_mediated 

percentage_mediated_fun(hist_data_df)

def boot_CI_fun(dat_df, metric_fun, B = 100):
  #Setting sample size
  N = len(dat_df)
  conf_level = 0.9
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
#boot_CI_fun(hist_data_df, percentage_mediated_fun)

#### 1.4. Reducing Uncertainty in Coefficients ####

# NEED MORE WORK HERE

##### 2. Instrumental variables #####

#Common libraries
import os
import pandas as pd
from statsmodels.formula.api import ols
import numpy as np

#Libraries for IV regressions
from linearmodels.iv import IV2SLS

#Loading the data from the chapter 10, then setting the right folder

os.chdir('C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\Behavioral data science book\\R scripts\\Part III Experimental design\\Chapter 10 - cluster randomization and hierarchical modeling')
hist_data_df = pd.read_csv('chap10-historical_data.csv')
exp_data_df = pd.read_csv('chap10-experimental_data.csv')
os.chdir('C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\Behavioral data science book\\R scripts\\Part IV Advanced tools\\Chapter 12 - Mediation and Instrumental Variables')

#Reformat group variable to binary in experimental data
exp_data_df.group = np.where(exp_data_df.group == 'treat', 1, 0)


#Reduced regression, coeff = 1.6
red_mod = ols("M6Spend~group+age+reason", data=exp_data_df).fit(disp=0)
red_mod.summary()

#First stage regression, coeff = 0.5
S1_mod = ols("call_CSAT~group+age+reason", data=exp_data_df).fit(disp=0)
S1_mod.summary()

#Baseline (biased) regression, coeff = 4.00
lm_mod = ols("M6Spend~call_CSAT+age+reason", data=exp_data_df).fit(disp=0)
lm_mod.summary()

#IV regression, coeff = 2.99
iv_mod = IV2SLS.from_formula('M6Spend ~ 1 + age + reason + [call_CSAT ~ group]', 
                             exp_data_df).fit()
iv_mod.params





















hist_data_df['age_quart'] = pd.cut(hist_data_df['age'], 4, 
                                   labels=['q4', 'q3', 'q2', 'q1'],
                                   include_lowest=True)

hist_data_df.groupby('play_area').agg(M = ('duration', lambda x: x.mean()),
    SD = ('duration', lambda x: x.std()))
                                     
#### Multiple moderators ####

# Measure coefficient for moderated moderation
ols("duration~play_area * children * age", data=hist_data_df).fit().summary()

# Determine 90%-CI for moderated moderation coefficient
def metric_fun(dat_df):
    model = ols("duration~play_area * children * age", data=dat_df)
    res = model.fit(disp=0)
    coeff = res.params['play_area:children:age']
    return coeff
#metric_fun(hist_data_df)



#boot_CI_fun(hist_data_df, metric_fun)

#### Validating moderation with Bootstrap ####

#Changing the metric function
def metric_fun2(dat_df):
    model = ols("duration~play_area * children", data=dat_df)
    res = model.fit(disp=0)
    coeff = res.params['play_area:children']
    return coeff
metric_fun(hist_data_df)

# Changing the bootstrap function
def boot_CI_fun2(dat_df, metric_fun, B = 100):
  #Setting the number of bootstrap samples
  
  #Setting sample size
  N = 10000
  conf_level = 0.9
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

boot_CI_fun2(hist_data_df, metric_fun2, B = 1000)

### Interpreting individual coefficients ###

# Centering the age variable in the historical data
centered_data_df = hist_data_df.copy()
centered_data_df['age'] = centered_data_df['age'] \
    .subtract(centered_data_df['age'].mean())

ols("duration~play_area * age", data=hist_data_df).fit().summary()
ols("duration~play_area * age", data=centered_data_df).fit().summary()

# Changing the default for PlayArea to 1
centered_data_df['play_area'] = centered_data_df['play_area']

ols("duration~play_area * age", data=hist_data_df).fit().summary()
ols("duration~play_area * age", data=centered_data_df).fit().summary()


### Calculating effects at the level of business decisions

def business_metric_fun(dat_df):
    
    model =  ols("duration~play_area * (children + age)", data=dat_df)
    res = model.fit(disp=0)
    
    action_dat_df = dat_df[dat_df.play_area == 0].copy()
    action_dat_df['pred_dur0'] = res.predict(action_dat_df) 
    action_dat_df.play_area = 1
    action_dat_df['pred_dur1'] = res.predict(action_dat_df)
    action_dat_df['pred_dur_diff'] = \
        action_dat_df.pred_dur1 - action_dat_df.pred_dur0
    action_res_df = action_dat_df.groupby(['store_id']) \
        .agg(mean_dur_diff=('pred_dur_diff', 'mean'), 
             tot_dur_diff=('pred_dur_diff', 'sum'))
    
    return action_res_df

action_res_df = business_metric_fun(hist_data_df)
action_res_df.describe()





