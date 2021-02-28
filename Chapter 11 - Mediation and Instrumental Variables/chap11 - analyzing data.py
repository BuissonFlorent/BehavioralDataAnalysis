# -*- coding: utf-8 -*-
"""
Created on Sun May 10 11:44:30 2020

@author: Florent
"""

#################################
##### This script analyzes the data used in chapter 11, 
##### Mediation and Instrumental Variables
#################################

##### Setup #####

#Common libraries
import pandas as pd
from statsmodels.formula.api import ols
import numpy as np


#Loading the data from the chapter on moderation
hist_data_df = pd.read_csv('chap10-historical_data.csv')

##### 1. Mediation #####

#Regressions used in the text
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

##### 2. Instrumental variables #####

#Common libraries
import pandas as pd
from statsmodels.formula.api import ols
import numpy as np

#Library for IV regressions
from linearmodels.iv import IV2SLS

#Loading the experimental data from the chapter 9
exp_data_df = pd.read_csv('chap10-experimental_data.csv')

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