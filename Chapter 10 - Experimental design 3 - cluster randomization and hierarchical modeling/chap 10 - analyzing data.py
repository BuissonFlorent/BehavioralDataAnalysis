# -*- coding: utf-8 -*-
"""
Created on Sun May 10 11:44:30 2020

@author: Florent
"""

import os
import pandas as pd
import numpy as np
import statsmodels.formula.api as sm
from statsmodels.formula.api import ols
from sklearn.preprocessing import MinMaxScaler
import itertools



os.chdir('C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\Behavioral data science book\\R scripts\\Part III Experimental design\\Chapter 9 - cluster randomization and hierarchical modeling')

##### Data and libraries #####

hist_data_df = pd.read_csv('chap9-historical_data.csv')
exp_data_df = pd.read_csv('chap9-experimental_data.csv')

#Shifting center_ID to distinguish it from indices
hist_data_df['center_ID'] = hist_data_df['center_ID'] + 100 
exp_data_df['center_ID'] = exp_data_df['center_ID'] + 100 

#Removing the variable M6Spend we won't use in this chapter
del(hist_data_df['M6Spend'])
del(exp_data_df['M6Spend'])

##### Introduction to hierarchical modeling #####

#Hierarchical analysis of historical data with center ID only as clustering variable
import statsmodels.formula.api as sm
mixed = sm.mixedlm("call_CSAT ~ reason + age", data = hist_data_df, 
                   groups = hist_data_df["center_ID"])
print(mixed.fit().summary())

#Hierarchical analysis of historical data with center ID and rep ID 
as clustering variable
vcf = {"rep_ID": "0+C(rep_ID)"}
mixed2 = sm.mixedlm("call_CSAT ~ reason + age", 
                    data = hist_data_df, 
                    groups = hist_data_df["center_ID"],
                    re_formula='1',
                    vc_formula=vcf)
print(mixed2.fit().summary())



##### Determining random assignment and sample size/power #####

### Metric function ###

def hlm_metric_fun(dat_df):
    vcf = {"rep_ID": "0+C(rep_ID)"}
    h_mod = sm.mixedlm("call_CSAT ~ reason + age + group", 
                    data = dat_df, 
                    groups = dat_df["center_ID"],
                    re_formula='1',
                    vc_formula=vcf)
    coeff = h_mod.fit().fe_params.values[2]
    return coeff
hlm_metric_fun(exp_data_df)   

# Metric function for free cleaning (treatment 1)
def lm_metric_fun(dat_df):
    model = ols("call_CSAT ~ reason + age + group", data=dat_df)
    res = model.fit(disp=0)
    coeff = res.params['group[T.treat]']
    return coeff
lm_metric_fun(exp_data_df) 

def boot_CI_fun(dat_df, metric_fun, B = 20, conf_level = 9/10):
  
  coeff_boot = []
  
  # Calculate coeff of interest for each simulation
  for b in range(B):
      print("beginning iteration number " + str(b) + "\n")
      boot_df = dat_df.groupby("rep_ID").sample(n=1000, replace=True).reset_index(drop= True)
      coeff = metric_fun(boot_df)
      coeff_boot.append(coeff)
  
  #Extract confidence interval
  coeff_boot.sort()
  offset = round(B * (1 - conf_level) / 2)
  confint = [coeff_boot[offset], coeff_boot[-(offset+1)]]
      
  return confint
#CI = boot_CI_fun(exp_data_df, hlm_metric_fun)

def decision_fun(dat_df, metric_fun, B = 20, conf_level = 0.9):
    boot_CI = boot_CI_fun(dat_df, metric_fun, B = B, conf_level = conf_level)
    decision = 1 if boot_CI[0] > 0  else 0
    return decision
decision_fun(exp_data_df, lm_metric_fun)


#### Stratified random assignment ####

### Function to prep the data
def strat_prep_fun(dat_df):
    #Getting variables at the center level
    dat_df = dat_df.groupby('center_ID').agg(
        nreps = ('rep_ID', lambda x: x.nunique()),
        avg_call_CSAT = ("call_CSAT", "mean"),
        avg_age=("age", "mean"),
        pct_reason_pmt=('reason', lambda x: sum(1 if r=='payment' else 0 for r in x)/len(x))
        )
    
    #Reformatting variables as needed
    dat_df['nreps'] = dat_df.nreps.astype(float)
    
    #Extracting components of the data
    num_df = dat_df.copy().loc[:,dat_df.dtypes=='float64'] #Numeric vars
    center_ID = [i for i in dat_df.index]

    #Normalizing all numeric variables to [0,1]
    scaler = MinMaxScaler()
    scaler.fit(num_df)
    num_np = scaler.transform(num_df)
    
    return center_ID, num_np
#center_ID, prepped_data_np = strat_prep_fun(hist_data_df)
    
def stratified_assgnt_fun(dat_df, K = 2):
    
    match_len = K - 1 # Number of matches we want to find
    match_idx = match_len - 1 # Accounting for 0-indexing
    
    center_ID, data_np = strat_prep_fun(dat_df)
    N = len(data_np)
    
    #Calculate distance matrix
    from scipy.spatial import distance_matrix
    d_mat = distance_matrix(data_np, data_np)
    np.fill_diagonal(d_mat,N+1)
    # Set up variables
    available = [i for i in range(N)]
    available_temp = available.copy()
    matches_lst = []
    lim = int(N/match_len)
    
    closest = np.argpartition(d_mat, kth=match_idx,axis=1)
    
    for n in available:
        if len(matches_lst) == lim: break
        if n in available_temp:
            for match_lim in range(match_idx,N-1):
                possible_matches = closest[n,:match_lim].tolist()
                matches = list(set(available_temp) & set(possible_matches))
                if len(matches) == match_len:
                    matches.append(n)
                    matches_lst.append(matches)
                    available_temp = [m for m in available_temp if m not in matches]
                    break
                else:
                    closest[n,:] = np.argpartition(d_mat[n,:], kth=match_lim)
    #Map center indices to their proper IDs
    matches_id_lst = [[center_ID[k[0]],center_ID[k[1]]] for k in matches_lst]
    return np.array(matches_id_lst)
stratified_pairs = stratified_assgnt_fun(hist_data_df, K=2)


##### Simulation function #####
def power_sim_fun(dat_df, metric_fun = lm_metric_fun, Nexp = 1000, eff_size = 1, B = 20, conf_level = 0.9):
    
    #Extract the stratified pairs
    stratified_pairs = stratified_assgnt_fun(dat_df, K=2)
    Npairs = len(stratified_pairs)
    Nperm = 2 ** Npairs
    power_list = []
    
    for m in dat_df.month.unique():
        #Sample down the data
        sample_data_df = dat_df.loc[dat_df.month==m,]
        sample_data_df = sample_data_df.groupby('rep_ID').sample(n=Nexp)\
            .reset_index(drop = True)
        for perm in range(Nperm):
            bin_str = f'{perm:0{Npairs}b}'
            idx = np.array([[i for i in range(Npairs)],
                            [int(d) for d in bin_str]]).T
            treat = [stratified_pairs[tuple(idx[i])] for i in range(Npairs)]
            
            sim_data_df = sample_data_df.copy()
            sim_data_df['group'] = 'ctrl'
            sim_data_df.loc[(sim_data_df.center_ID.isin(treat)),'group']\
                = 'treat'
            
            sim_data_df.loc[(sim_data_df.group=='treat'),'call_CSAT'] =\
                sim_data_df.loc[(sim_data_df.group=='treat'),'call_CSAT'] + eff_size
                
            sim_data_df.loc[(sim_data_df.call_CSAT > 10), 'call_CSAT'] = 10
            
            # Option 1: extract CIs for visualization
            #sim_CI = boot_CI_fun(sim_data_df, lm_metric_fun)
            #power_list.append(sim_CI)
            
            # Option 2: calculate decision for overall power determination
            D = decision_fun(sim_data_df, metric_fun, B = B, conf_level = conf_level)
            power_list.append(D)
            
    return power_list




##### Analysis of experimental data #####
vcf = {"rep_ID": "0+C(rep_ID)"}
h_mod = sm.mixedlm("call_CSAT ~ reason + age + group", 
                   data = exp_data_df, 
                   groups = exp_data_df["center_ID"],
                   vc_formula=vcf)
print(h_mod.fit().summary())


