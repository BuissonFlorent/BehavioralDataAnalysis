# -*- coding: utf-8 -*-
"""
Created on Sun May 10 11:44:30 2020

@author: Florent
"""

import os
import pandas as pd
import numpy as np
import random
from statsmodels.formula.api import ols
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import OneHotEncoder

dir_string = ("C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\"
              "Behavioral data science book\\R scripts\\"
              "Part III Experimental design\\"
              "Chapter 9 - stratified randomization")
os.chdir(dir_string)

##### Loading the data #####

hist_data_df = pd.read_csv('chap8-historical_data.csv')
exp_data_df = pd.read_csv('chap8-experimental_data.csv')

#Restating tier as a factor variable
# =============================================================================
# hist_data_df['tier'] = pd.Categorical(hist_data_df.tier, 
#                                       categories=[3,2,1], 
#                                       ordered = True)
# exp_data_df['tier'] = pd.Categorical(exp_data_df.tier, 
#                                      categories=[3,2,1], 
#                                      ordered = True)
# =============================================================================

##### Determining random assignment and sample size/power #####

#### Random assignment ####

# Function for assignment completely at random with 3 experimental groups
def no_strat_assgnt_fun(dat_df, Nexp):
    K = 3
    dat_df = pd.DataFrame({'ID': dat_df.ID.unique()})
    dat_df = dat_df.sample(Nexp)
    dat_df['assgnt'] = np.random.uniform(0,1,Nexp)
    dat_df['group'] = 'ctrl'
    dat_df.loc[dat_df['assgnt'].between(0, 1/K, inclusive=True), 
               'group'] = 'treat1'
    dat_df.loc[dat_df['assgnt'].between(1/K, 2/K, inclusive=False), 
               'group'] = 'treat2'
    del(dat_df['assgnt'])
    return dat_df
#no_strat_assgnt = no_strat_assgnt_fun(hist_data_df, Nexp = 4998)

# Extension of the previous function for any number K
def no_strat_assgnt_K_fun(dat_df, Nexp, K):
    dat_df = pd.DataFrame({'ID': dat_df.ID.unique()})
    dat_df = dat_df.sample(Nexp)
    dat_df['assgnt'] = np.random.uniform(0,1,Nexp)
    dat_df['group'] = -1 # initializing the “group” variable
    for i in range(K):
        dat_df.loc[dat_df['assgnt'].between(i/K, (i+1)/K, inclusive=True), 
               'group'] = i
    del(dat_df['assgnt'])
    return dat_df   
#no_strat_assgnt = no_strat_assgnt_K_fun(hist_data_df, Nexp = 5000, K = 4)

### Function to prep the data
def strat_prep_fun(dat_df):
    #Extracting property-level variables
    dat_df['tier'] = pd.Categorical(dat_df.tier, categories=[3,2,1], 
                                    ordered = True)
    dat_df['ID'] = dat_df.ID.astype(str)
    
    
    num_df = dat_df.copy().loc[:,dat_df.dtypes=='float64'] #Numeric vars 
    cat_df = dat_df.copy().loc[:,dat_df.dtypes=='category'] #Categorical vars

    #Normalizing all numeric variables to [0,1]
    scaler = MinMaxScaler()
    scaler.fit(num_df)
    num_np = scaler.transform(num_df)
    
    #One-hot encoding all categorical variables
    enc = OneHotEncoder(handle_unknown='ignore')
    enc.fit(cat_df)
    cat_np = enc.transform(cat_df).toarray()
    
    #Binding arrays
    data_np = np.concatenate((num_np, cat_np), axis=1)
    del num_df, num_np, cat_df, cat_np, enc, scaler
    return data_np
#prepped_data_np = strat_prep_fun(hist_data_df)
    
def stratified_assgnt_fun(dat_df, K = 2):
      
    dat_ID = dat_df.ID.astype(str).tolist() # Extract ID for later join

    match_len = K - 1 # Number of matches we want to find
    match_idx = match_len - 1 # Accounting for 0-indexing
    
    data_np = strat_prep_fun(dat_df)
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
        #print("n = ", n)
        if len(matches_lst) == lim: break
        if n in available_temp:
            for match_lim in range(match_idx,N-1):
                #print("match_lim = ", match_lim)
                possible_matches = closest[n,:match_lim].tolist()
                matches = list(set(available_temp) & set(possible_matches))
                #print("len(matches) = ",  len(matches))
                if len(matches) == match_len:
                    matches.append(n)
                    matches_lst.append(matches)
                    available_temp = [m for m in available_temp if m not in matches]
                    break
                else:
                    closest[n,:] = np.argpartition(d_mat[n,:], kth=match_lim)
                    
    #Assigning experimental groups to the matched sets
    exp_grps = np.array(list(range(K))*(int(N/K))).reshape((int(N/K),K))
    exp_grps = exp_grps.tolist()
    for j in exp_grps: 
        np.random.shuffle(j)
    #flattening the two lists
    import itertools
    exp_grps = list(itertools.chain(*exp_grps))
    matches_lst2 = list(itertools.chain(*matches_lst))
    exp_grps2 = [x for _,x in sorted(zip(matches_lst2,exp_grps))]
    
    assgnt_df = pd.DataFrame(exp_grps2, columns=['group'])
    assgnt_df.group = assgnt_df.group.astype(str)
    assgnt_df.group.loc[assgnt_df.group == '0'] = 'ctrl'
    assgnt_df.group.loc[assgnt_df.group == '1'] = 'treat1'
    assgnt_df.group.loc[assgnt_df.group == '2'] = 'treat2'
    
    
    assgnt_df['ID'] = dat_ID
    dat_df = dat_df.merge(assgnt_df, on='ID', how='inner')
    return dat_df

#Sampling a random monthly period
per = random.sample(range(35), 1)[0] + 1
sample_df = hist_data_df.loc[hist_data_df.period == per].sample(4998)
stratified_data_df = stratified_assgnt_fun(sample_df, K=3)

def assgnt_comparison_fun(strat_dat_df, varnm):
    
    strat_dat_df = stratified_data_df.copy()
    K = 3
    strat_dat_df.rename(columns = {'group':'strat_group'}, inplace=True)
    strat_dat_df['assgnt'] = np.random.uniform(0,1,len(strat_dat_df))
    strat_dat_df['group'] = -1 # initializing the “group” variable
    for i in range(K):
        strat_dat_df.loc[strat_dat_df['assgnt'].between(i/K, (i+1)/K, inclusive=True), 
               'group'] = i
    del(strat_dat_df['assgnt'])
    strat_dat_df.rename(columns = {'group':'no_strat_group'}, inplace=True)
    
    strat_sd = round(float(strat_dat_df.groupby('strat_group').agg(var = (varnm, 'mean')).std()), 4)
    print("the s.d. between groups for", varnm, "is", strat_sd, 
          " for stratified assignment\n")
    no_strat_sd = round(float(strat_dat_df.groupby('no_strat_group').agg(var = (varnm, 'mean')).std()),4)
    print("the s.d. between groups for", varnm, "is", no_strat_sd, 
          "for non-stratified assignment\n") 

assgnt_comparison_fun(stratified_data_df, 'avg_review')
assgnt_comparison_fun(stratified_data_df, 'sq_ft')
assgnt_comparison_fun(stratified_data_df, 'BPday')

##### Simulations for power analysis #####

# Metric function for free cleaning (treatment 1)
def treat1_metric_fun(dat_df):
    model = ols("BPday~sq_ft+tier+avg_review+group", data=dat_df)
    res = model.fit(disp=0)
    coeff = res.params['group[T.treat1]']
    return coeff
    
# Metric function for minimum booking duration (treatment 2)
def treat2_metric_fun(dat_df):
    model = ols("BPday~sq_ft+tier+avg_review+group", data=dat_df)
    res = model.fit(disp=0)
    coeff = res.params['group[T.treat2]']
    return coeff

def boot_CI_fun(dat_df, metric_fun, B = 100, conf_level = 0.9):
  #Setting sample size
  N = len(dat_df)
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

def decision_fun(dat_df, metric_fun, B = 100, conf_level = 0.9):
    boot_CI = boot_CI_fun(dat_df, metric_fun, B = B, conf_level = conf_level)
    decision = 1 if boot_CI[0] > 0  else 0
    return decision
#decision_fun(exp_data_df, treat2_metric_fun)    

### Function for single experiment

def single_sim_fun(dat_df, metric_fun, Nexp, eff_size, B = 100, 
                   conf_level = 0.9):
    
    #Filter the data down to a random month
    per = random.sample(range(35), 1)[0] + 1
    dat_df = dat_df.loc[dat_df.period == per]
    dat_df = dat_df.sample(n=Nexp)
    
    #Prepare the stratified assignment for a random sample of desired size 
    sample_df = dat_df.sample(Nexp)
    sim_data_df = stratified_assgnt_fun(sample_df, K = 3)
    
    #Add target effect size
    sim_data_df.BPday = np.where(sim_data_df.group == 'treat2', 
                                 sim_data_df.BPday + eff_size, sim_data_df.BPday)
    
    #Calculate the decision (we want it to be 1)
    decision = decision_fun(sim_data_df, metric_fun, B = B, 
                            conf_level = conf_level)
    return decision
    
single_sim_fun(hist_data_df, treat2_metric_fun, Nexp=99, eff_size=2)

### Functions for simulations at scale
#Standard function
def power_sim_fun(dat_df, metric_fun, Nexp, eff_size, Nsim, B = 100, 
                  conf_level = 0.9):
    power_lst = []
    for i in range(Nsim):
        power_lst.append(single_sim_fun(dat_df, metric_fun = metric_fun, 
                                        Nexp = Nexp, eff_size = eff_size, 
                                        B = B, conf_level = conf_level))
    power = np.mean(power_lst)
    return(power)  
power = power_sim_fun(hist_data_df, treat2_metric_fun, Nexp = 1500, eff_size = 2, 
                      Nsim = 100, B = 100, conf_level = 0.9)

##### Analyzing and interpreting experimental results #####

#Restating tier as a factor variable
exp_data_df['tier'] = pd.Categorical(exp_data_df.tier, 
                                      categories=[3,2,1], 
                                      ordered = True)

#Linear regression
exp_data_reg_df = exp_data_df.copy()
exp_data_reg_df.BPday = np.where((exp_data_reg_df.compliant == 1) & \
                                 (exp_data_reg_df.group == 'treat2'), 
                                 exp_data_reg_df.BPday -10, 
                                 exp_data_reg_df.BPday) 
print(ols("BPday~sq_ft+tier+avg_review+group", 
          data=exp_data_reg_df).fit(disp=0).summary())
boot_CI_fun(exp_data_reg_df, treat1_metric_fun)
boot_CI_fun(exp_data_reg_df, treat2_metric_fun)

#T-test of means for treatment 1
from statsmodels.stats.weightstats import ttest_ind
test = ttest_ind(exp_data_df[exp_data_df.group == 'ctrl']['BPday'], 
                 exp_data_df[exp_data_df.group == 'treat1']['BPday'], 
                 alternative = 'smaller')

#Measurinng the compliance rate
exp_data_reg_df.groupby('group').agg(compliance_rate = ('compliant', 'mean'))

