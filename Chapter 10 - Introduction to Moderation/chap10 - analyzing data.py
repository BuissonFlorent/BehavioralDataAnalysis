# -*- coding: utf-8 -*-
"""
Created on Sun May 10 11:44:30 2020

@author: Florent
"""

import os
import pandas as pd
from statsmodels.formula.api import ols
import numpy as np


os.chdir('C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\Behavioral data science book\\R scripts\\Part IV Advanced tools\\Chapter 11 - Introduction to moderation')

##### Data and libraries #####

hist_data_df = pd.read_csv('chap11-historical_data.csv')

##### Varieties of moderation #####

ols("duration~play_area * children", data=hist_data_df).fit().summary()

##### How to apply moderation #####

#### When to look for moderation ####

#Code to convert age to its quartiles

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





