# -*- coding: utf-8 -*-
"""
Created on Sun May 10 11:44:30 2020

@author: Florent
"""

import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import random
from statsmodels.formula.api import ols
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import OneHotEncoder


os.chdir('C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\Behavioral data science book\\R scripts\\Part III Experimental design\\Chapter 9 - online population-based exp')

##### Loading the data #####

hist_data_df = pd.read_csv('chap9-historical_data.csv')
exp_data_df = pd.read_csv('chap9-experimental_data.csv')

#Restating tier as a factor variable
hist_data_df['tier'] = pd.Categorical(hist_data_df.tier, 
                                      categories=[3,2,1], 
                                      ordered = True)
exp_data_df['tier'] = pd.Categorical(exp_data_df.tier, 
                                     categories=[3,2,1], 
                                     ordered = True)


##### Random assignment #####

#Code for random assignment without stratification - manual approach
s = 10000
sample_df = hist_data_df.sample(s)
sample_df['assgnt'] = np.random.uniform(0,1,s)
def assgnt_fun(assgnt):
    k = 3
    if (assgnt <= 1/k):
        return 'ctrl'
    elif (assgnt <= 2/k):
        return 'treat1'
    else:
        return 'treat2' 
sample_df['group'] = sample_df['assgnt'].apply(assgnt_fun)

#Code for random assignment without stratification - automated approach
s = 2001
sample_df = hist_data_df.sample(s)
sample_df['assgnt'] = np.random.uniform(0,1,s)
k = 3
def assgnt_fun(assgnt):
    for i in range(k):
        if (assgnt <= (1+i)/k):
            return i
sample_df['group'] = sample_df['assgnt'].apply(assgnt_fun)

### Stratified sampling 

def stratified_assgnt_fun(df=hist_data_df,N=60,k=3):
    #Creating new datasets for stratification purposes
    sample_df = df.copy().sample(N)
    num_df = sample_df.copy().loc[:,sample_df.dtypes=='float64'] #Numeric vars only
    cat_df = sample_df.copy().loc[:,sample_df.dtypes=='category'] #Categorical vars only

    #Normalizing all numeric variablesto [0,1]
    
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
    
    #Calculate distance matrix
    from scipy.spatial import distance_matrix
    d_mat = distance_matrix(data_np, data_np)
    np.fill_diagonal(d_mat,9)
    
    #Naive greedy matching algorithm
    
    idx = np.argpartition(d_mat, kth=k,axis=1)
    idx
    
    match_lst =[]
    avail_idx = {i for i in range(N)}
    for i in range(N):
        #print("Iteration number ", i)
        if i in avail_idx:
            match_lim = k - 1
            matches_found = 0
            for match_lim in list(range(k-1,N+1)):
                #print("match_lim = ", match_lim)
                #print("matches found: ", matches_found)
                possib_matches = idx[i,:match_lim].tolist()
                matches = avail_idx.intersection(possib_matches)
                matches_found = len(matches)
                if matches_found == k - 1:
                    #print("matches found: ", matches_found)
                    break
                #print("Updating idx matrix")
                idx[i,:] = np.argpartition(d_mat[i,:], kth=match_lim,axis=0)
    
            match_lst.append([i] + list(matches))
            avail_idx.remove(i)
            for t in matches:
                avail_idx.remove(t)
    
    #Assigning experimental groups to the matched sets
    exp_grps = np.array([0,1,2]*N).reshape((N,3))
    exp_grps = exp_grps.tolist()
    for j in exp_grps: 
        np.random.shuffle(j)
    #flattening the two lists
    import itertools
    exp_grps = list(itertools.chain(*exp_grps))
    match_lst = list(itertools.chain(*match_lst))
    
    assgnt = list(zip(match_lst,exp_grps))
    from operator import itemgetter
    assgnt.sort(key=itemgetter(0))
    assgnt_lst = [tup[1] for tup in assgnt]
    sample_df['strat_assgnt'] = assgnt_lst
    
    return sample_df

N = 2001 #Number of subjects required for experiment
k = 3 #Number of experimental groups required (incl. control group)
sample_df = stratified_assgnt_fun(hist_data_df,N,k)


# Compare random and stratified assignments

def assgnt_comparison_fun(sample_df, numvar):
    sample_df['rnd_assgnt'] = [random.randint(0,2) for iter in range(N)]
    t1 = sample_df.groupby('strat_assgnt').agg(mean = (numvar, 'mean'))
    t2 = sample_df.groupby('rnd_assgnt').agg(mean = (numvar, 'mean'))
    print("The s.d. between groups for", numvar, "is", round(t1['mean'].std(),1), 
          "for stratified assignment")
    print("The s.d. between groups for", numvar, "is", round(t2['mean'].std(),1), 
          "for random assignment")
    
assgnt_comparison_fun(sample_df, 'sq_ft')
assgnt_comparison_fun(sample_df, 'BPday')
assgnt_comparison_fun(sample_df, 'avg_review')

##### Power analysis simulations #####


### Statistical significance analysis of historical data - single run

#Sampling historical data and generating stratified random group 
random.seed(123)
np.random.seed(123)
N = 2001 #Number of subjects required for experiment
k = 3 #Number of experimental groups required (incl. control group)
sample_df = stratified_assgnt_fun(hist_data_df,N,k)
sample_df['group_strat'] = sample_df['strat_assgnt']

#Generating unstratified random group assignment without any effect
sample_df['assgnt'] = np.random.uniform(0,1,N)
def assgnt_fun(assgnt):
    for i in range(k):
        if (assgnt <= (1+i)/k):
            return i
sample_df['group_nostrat'] = sample_df['assgnt'].apply(assgnt_fun)
#Removing intermediary variables
sample_df = sample_df.drop(['assgnt','strat_assgnt'], axis = 1)

#Running linear regression on stratified data
sample_df['tier'] = pd.Categorical(sample_df.tier, 
                                   categories=[1,2,3], 
                                   ordered = True)
sample_df['group_strat'] = pd.Categorical(sample_df.group_strat, 
                                          categories=[0,1,2], 
                                          ordered = True)
lin_model_strat = ols("BPday~sq_ft+tier+avg_review+group_strat", 
                      data=sample_df)
print(lin_model_strat.fit().summary())

#Running linear regression on non-stratified data
sample_df['group_nostrat'] = pd.Categorical(sample_df.group_strat, 
                                          categories=[0,1,2], 
                                          ordered = True)
lin_model_nostrat = ols("BPday~sq_ft+tier+avg_review+group_nostrat", 
                      data=sample_df)
print(lin_model_nostrat.fit().summary())


### Statistical significance analysis of historical data - repeated runs
random.seed(123)
np.random.seed(123)

nloops = 40
sample_sizes = [N] * nloops

def false_positive_repeat_sim(sample_sizes):
    #Serialized function with FOR loop
    strat_coeff1_vector = []
    strat_pvalue1_vector = []
    nostrat_coeff1_vector = []
    nostrat_pvalue1_vector = []  
 
    for s in sample_sizes:
        #Sampling historical data and generating stratified random group 
        #assignment
        sample_df = stratified_assgnt_fun(hist_data_df,s,k=k)
        sample_df['group_strat'] = sample_df['strat_assgnt']
        
        #Generating unstratified random group assignment without any effect
        sample_df['assgnt'] = np.random.uniform(0,1,N)
        sample_df['group_nostrat'] = sample_df['assgnt'].apply(assgnt_fun)
        sample_df = sample_df.drop(['assgnt','strat_assgnt'], axis = 1)
        
        #Running linear regression on stratified data
        sample_df['group_strat'] = pd.Categorical(sample_df.group_strat, 
                                                  categories=[0,1,2], 
                                                  ordered = True)
        lin_model_strat = ols("BPday~sq_ft+tier+avg_review+group_strat", 
                              data=sample_df)
     
        res_strat = lin_model_strat.fit(disp=0)
        
        #Running linear regression on non-stratified data
        sample_df['group_nostrat'] = pd.Categorical(sample_df.group_nostrat, 
                                                  categories=[0,1,2], 
                                                  ordered = True)
        lin_model_nostrat = ols("BPday~sq_ft+tier+avg_review+group_nostrat", 
                              data=sample_df)
     
        res_nostrat = lin_model_nostrat.fit(disp=0)
    
        # Extracting the coefficients of interest from the summary
        strat_coeff1_vector.append(res_strat.params['group_strat[T.1]'])
        strat_pvalue1_vector.append(res_strat.pvalues['group_strat[T.1]'])
        nostrat_coeff1_vector.append(res_nostrat.params['group_nostrat[T.1]'])
        nostrat_pvalue1_vector.append(res_nostrat.pvalues['group_nostrat[T.1]'])
        
    sim_summary_df = pd.DataFrame({'sample_size' : sample_sizes,
                                   'strat_coeff1': strat_coeff1_vector,
                                   'strat_pvalue1': strat_pvalue1_vector,
                                   'nostrat_coeff1': nostrat_coeff1_vector,
                                   'nostrat_pvalue1': nostrat_pvalue1_vector
                                   })
    return sim_summary_df                                       
sim_summary_df = false_positive_repeat_sim(sample_sizes)

#Determining the empirical stat. sig. thereshold visually (N=2,001)
fig, (ax1, ax2) = plt.subplots(ncols=2)
ax1.set_title("empirical stat. sig. with \nnon-stratified data") 
ax1.set_xlim(-2,2)
ax1.set_ylim(0,1)
ax2.set_title("empirical stat. sig. with \nstratified data") 
ax2.set_xlim(-2,2)
ax2.set_ylim(0,1)
ax2.set_yticklabels([])
sns.scatterplot(x='nostrat_coeff1', y='nostrat_pvalue1', data=sim_summary_df, 
                ax=ax1)
ax1.axhline(y=0.1, color='r')
ax1.set_xlabel('estimated coeff. for treat. 1')
ax1.set_ylabel('estimated p-value')
sns.scatterplot(x='strat_coeff1', y='strat_pvalue1', data=sim_summary_df, 
                ax=ax2)
ax2.axhline(y=0.1, color='r')
ax2.set_xlabel('estimated coeff. for treat. 1')
ax2.set_ylabel('estimated p-value')
fig.tight_layout()
plt.show()


### Repeated simulation for false negatives ($2 true effect)(N=2001)
random.seed(123)
np.random.seed(123)

nloops = 40
sample_sizes = [N] * nloops

def false_negative_repeat_sim(sample_sizes):
    #Setting the true effect size
    eff_size = 2
    
    #Serialized function with FOR loop
    strat_coeff1_vector = []
    strat_pvalue1_vector = []
    nostrat_coeff1_vector = []
    nostrat_pvalue1_vector = []  
    
 
    for s in sample_sizes:
        #Sampling historical data and generating stratified random group 
        #assignment with a $2 effect
        sample_df = stratified_assgnt_fun(hist_data_df,N=s,k=k)
        sample_df['group_strat'] = sample_df['strat_assgnt']
        sample_df['BPday_strat'] = np.where(sample_df['group_strat'] == 1, 
                                            sample_df['BPday'] 
                                            + np.random.normal(eff_size,1,s), 
                                            sample_df['BPday'])
        
        #Generating unstratified random group assignment with a $2 effect
        sample_df['assgnt'] = np.random.uniform(0,1,N)
        sample_df['group_nostrat'] = sample_df['assgnt'].apply(assgnt_fun)
        sample_df['BPday_nostrat'] = np.where(sample_df['group_nostrat'] == 1, 
                                            sample_df['BPday'] 
                                            + np.random.normal(eff_size,1,s), 
                                            sample_df['BPday'])
        sample_df = sample_df.drop(['assgnt','strat_assgnt'], axis = 1)
        
        #Running linear regression on stratified data
        sample_df['group_strat'] = pd.Categorical(sample_df.group_strat, 
                                                  categories=[0,1,2], 
                                                  ordered = True)
        lin_model_strat = ols("BPday_strat~sq_ft+tier+avg_review+group_strat", 
                              data=sample_df)
     
        res_strat = lin_model_strat.fit(disp=0)
        
        #Running linear regression on non-stratified data
        sample_df['group_nostrat'] = pd.Categorical(sample_df.group_nostrat, 
                                                  categories=[0,1,2], 
                                                  ordered = True)
        lin_model_nostrat = ols("BPday_nostrat~sq_ft+tier+avg_review+group_nostrat", 
                              data=sample_df)
     
        res_nostrat = lin_model_nostrat.fit(disp=0)
    
        # Extracting the coefficients of interest from the summary
        strat_coeff1_vector.append(res_strat.params['group_strat[T.1]'])
        strat_pvalue1_vector.append(res_strat.pvalues['group_strat[T.1]'])
        nostrat_coeff1_vector.append(res_nostrat.params['group_nostrat[T.1]'])
        nostrat_pvalue1_vector.append(res_nostrat.pvalues['group_nostrat[T.1]'])
        
    sim_summary_df = pd.DataFrame({'strat_coeff1': strat_coeff1_vector,
                                   'strat_pvalue1': strat_pvalue1_vector,
                                   'nostrat_coeff1': nostrat_coeff1_vector,
                                   'nostrat_pvalue1': nostrat_pvalue1_vector
                                   })
    return sim_summary_df                                       
sim_summary_df = false_negative_repeat_sim(sample_sizes)

#Determining the empirical power thereshold visually (N=2,001)
fig, (ax1, ax2) = plt.subplots(ncols=2)
ax1.set_title("empirical stat. power with \nnon-stratified data") 
ax1.set_xlim(0,3)
ax1.set_ylim(0,1)
ax2.set_title("empirical stat. power with \nstratified data") 
ax2.set_xlim(0,3)
ax2.set_ylim(0,1)
ax2.set_yticklabels([])
sns.scatterplot(x='nostrat_coeff1', y='nostrat_pvalue1', data=sim_summary_df, 
                ax=ax1)
ax1.axhline(y=0.1, color='r')
ax1.set_xlabel('estimated coeff. for treat. 1')
ax1.set_ylabel('estimated p-value')
sns.scatterplot(x='strat_coeff1', y='strat_pvalue1', data=sim_summary_df, 
                ax=ax2)
ax2.axhline(y=0.1, color='r')
ax2.set_xlabel('estimated coeff. for treat. 1')
ax2.set_ylabel('estimated p-value')
fig.tight_layout()
plt.show()



### Statistical significance and power simulation at scale ($2 true effect)
### (multiple sample sizes)

#Generating a list of sample sizes (divisible by 3)
nloops_per_ss = 200
ss_base = range(99,2376,99)
temp_list = [[s] * nloops_per_ss for s in ss_base]
sample_sizes = [item for sublist in temp_list for item in sublist]
del nloops_per_ss, ss_base, temp_list

def power_sim(sample_sizes, eff_size = 2):
    
    #Serialized function with FOR loop
    strat_coeff1_noeff_vector = []
    strat_pvalue1_noeff_vector = []
    nostrat_coeff1_noeff_vector = []
    nostrat_pvalue1_noeff_vector = []  
    strat_coeff1_eff_vector = []
    strat_pvalue1_eff_vector = []
    nostrat_coeff1_eff_vector = []
    nostrat_pvalue1_eff_vector = [] 
    
 
    for s in sample_sizes:
        #Sampling historical data and generating stratified random group 
        #assignment with no effect yet
        sample_df = stratified_assgnt_fun(hist_data_df,N=s,k=k)
        sample_df['group_strat'] = sample_df['strat_assgnt']
        sample_df['group_strat'] = pd.Categorical(sample_df.group_strat, 
                                                  categories=[0,1,2], 
                                                  ordered = True)
        
        #Running linear regression on stratified data with no effect
        lin_model_strat_noeff = ols("BPday~sq_ft+tier+avg_review+group_strat", 
                              data=sample_df)
        res_strat_noeff = lin_model_strat_noeff.fit(disp=0)
        
        #Adding $2 effect to stratified data
        sample_df['BPday_strat'] = np.where(sample_df['group_strat'] == 1, 
                                            sample_df['BPday'] 
                                            + np.random.normal(eff_size,1,s), 
                                            sample_df['BPday'])
        
        #Running linear regression on stratified data with true effect
        lin_model_strat_eff = ols("BPday_strat~sq_ft+tier+avg_review+group_strat", 
                              data=sample_df)
        res_strat_eff = lin_model_strat_eff.fit(disp=0)
        
        #Generating unstratified random group assignment with no effect yet
        sample_df['assgnt'] = np.random.uniform(0,1,s)
        sample_df['group_nostrat'] = sample_df['assgnt'].apply(assgnt_fun)
        sample_df['group_nostrat'] = pd.Categorical(sample_df.group_nostrat, 
                                                  categories=[0,1,2], 
                                                  ordered = True)
        
        #Running linear regression on non-stratified data with no effect
        lin_model_nostrat_noeff = ols("BPday~sq_ft+tier+avg_review+group_nostrat", 
                              data=sample_df)
        res_nostrat_noeff = lin_model_nostrat_noeff.fit(disp=0)
        
        #Adding $2 effect to non-stratified data
        sample_df['BPday_nostrat'] = np.where(sample_df['group_nostrat'] == 1, 
                                            sample_df['BPday'] 
                                            + np.random.normal(eff_size,1,s), 
                                            sample_df['BPday'])
        sample_df = sample_df.drop(['assgnt','strat_assgnt'], axis = 1)
        
        #Running linear regression on non-stratified data with true effect
        lin_model_nostrat_eff = ols("BPday_nostrat~sq_ft+tier+avg_review+group_nostrat", 
                              data=sample_df)
        res_nostrat_eff = lin_model_nostrat_eff.fit(disp=0)
        
        
        # Extracting the coefficients of interest from the summary
        strat_coeff1_noeff_vector.append(res_strat_noeff.params['group_strat[T.1]'])
        strat_pvalue1_noeff_vector.append(res_strat_noeff.pvalues['group_strat[T.1]'])
        nostrat_coeff1_noeff_vector.append(res_nostrat_noeff.params['group_nostrat[T.1]'])
        nostrat_pvalue1_noeff_vector.append(res_nostrat_noeff.pvalues['group_nostrat[T.1]'])
        strat_coeff1_eff_vector.append(res_strat_eff.params['group_strat[T.1]'])
        strat_pvalue1_eff_vector.append(res_strat_eff.pvalues['group_strat[T.1]'])
        nostrat_coeff1_eff_vector.append(res_nostrat_eff.params['group_nostrat[T.1]'])
        nostrat_pvalue1_eff_vector.append(res_nostrat_eff.pvalues['group_nostrat[T.1]'])
        
    sim_summary_df = pd.DataFrame({'sample_size' : sample_sizes,
                                   'strat_coeff1_noeff': strat_coeff1_noeff_vector,
                                   'strat_pvalue1_noeff': strat_pvalue1_noeff_vector,
                                   'nostrat_coeff1_noeff': nostrat_coeff1_noeff_vector,
                                   'nostrat_pvalue1_noeff': nostrat_pvalue1_noeff_vector,
                                   'strat_coeff1_eff': strat_coeff1_eff_vector,
                                   'strat_pvalue1_eff': strat_pvalue1_eff_vector,
                                   'nostrat_coeff1_eff': nostrat_coeff1_eff_vector,
                                   'nostrat_pvalue1_eff': nostrat_pvalue1_eff_vector
                                   })
    return sim_summary_df                                       
sim_summary_df = power_sim(sample_sizes, eff_size=2)

#Determining empirical power of OLS with and without stratified data
sim_summary_df['pw_strat'] = np.where((sim_summary_df['strat_pvalue1_eff'] < 0.1) &
                                       (sim_summary_df['strat_coeff1_eff'] > 0), 1, 0)
sim_summary_df['pw_nostrat'] = np.where((sim_summary_df['nostrat_pvalue1_eff'] < 0.1) &
                                       (sim_summary_df['nostrat_coeff1_eff'] > 0), 1, 0)
pow_summary_df = sim_summary_df.groupby('sample_size').agg(count_TP_strat=('pw_strat',sum),
                                                           count_TP_nostrat=('pw_nostrat',sum),
                                          count_total = ('pw_strat', 'count'))
pow_summary_df['pw_strat'] = pow_summary_df['count_TP_strat'] / pow_summary_df['count_total']
pow_summary_df['pw_nostrat'] = pow_summary_df['count_TP_nostrat'] / pow_summary_df['count_total']

pow_summary_df = pow_summary_df.drop(['count_TP_strat','count_TP_nostrat','count_total'], axis=1)
pow_summary_df['sample_size'] = pow_summary_df.index
pow_summary_df

#Drawing results
fig, (ax1, ax2) = plt.subplots(ncols=2)
ax1.set_title("empirical stat. power with \nstratified data") 
ax1.set_xlim(600, 2400)
ax1.set_ylim(0,1)
ax1.set_xlabel('sample size')
ax1.set_ylabel('empirical power')
ax1.axhline(y=0.9, color='r')
sns.scatterplot(x='sample_size', y='pw_strat', data=pow_summary_df, 
                ax=ax1)
ax2.set_title("empirical stat. power with \nnon-stratified data") 
ax2.set_xlim(600, 2400)
ax2.set_ylim(0,1)
ax2.set_xlabel('sample size')
ax2.set_ylabel('empirical power')
ax2.axhline(y=0.9, color='r')
ax2.set_yticklabels([])
sns.scatterplot(x='sample_size', y='pw_nostrat', data=pow_summary_df, 
                ax=ax2)
fig.tight_layout()
plt.show()

##### Traditional statistical Power analysis #####

#Statistical power analysis
import statsmodels.stats.power as ssp
analysis = ssp.TTestIndPower()
analysis.solve_power(effect_size = 0.13, alpha = 0.1, nobs1 = None, 
                     alternative = 'two-sided', power=0.9)

##### Analyzing experimental data #####

#T-test of means for treatment 1
from statsmodels.stats.weightstats import ttest_ind
test = ttest_ind(exp_data_df[exp_data_df.group == 'ctrl']['BPday'], 
                 exp_data_df[exp_data_df.group == 'treat1']['BPday'], 
                 alternative = 'smaller')

#T-test of means for treatment 2
from statsmodels.stats.weightstats import ttest_ind
test = ttest_ind(exp_data_df[exp_data_df.group == 'ctrl']['BPday'], 
                 exp_data_df[exp_data_df.group == 'treat2']['BPday']-10, 
                 alternative = 'smaller')

### Linear regression

#Normalizing the profit per day
exp_data_reg_df = exp_data_df.copy()
exp_data_reg_df.loc[exp_data_reg_df.group == 'treat2','BPday'] = exp_data_reg_df.loc[exp_data_reg_df.group == 'treat2','BPday']-10

#Running the OLS regression
lin_model = ols("BPday~sq_ft+tier+avg_review+group", data=exp_data_reg_df)
res_lin = lin_model.fit()
print(res_lin.summary())

#Getting the confidence interval for treatment 2
res_lin.conf_int(alpha=0.10, cols=None).loc['group[T.treat2]',:]


