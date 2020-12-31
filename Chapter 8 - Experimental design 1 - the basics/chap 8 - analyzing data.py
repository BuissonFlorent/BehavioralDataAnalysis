# -*- coding: utf-8 -*-
"""
Created on Sun May 10 11:44:30 2020

@author: Florent
"""

import os
import pandas as pd

#Data analysis libraries
import numpy as np
import random
from math import asin, sqrt
import statsmodels.formula.api as smf
import statsmodels.stats.power as ssp
import statsmodels.stats.proportion as sspr

#Graphical libraries
import seaborn as sns


#Libraries required to parallelize simulations
from joblib import Parallel, delayed
import psutil


os.chdir('C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\Behavioral data science book\\R scripts\\Part III Experimental design\\Chapter 8 - online streaming exp')

##### Loading the data #####

hist_data_df = pd.read_csv('chap8-historical_data.csv')
exp_data_df = pd.read_csv('chap8-experimental_data.csv')

##### Code for random assignment #####

# K = 2
# assgnt = np.random.uniform(0,1,1)
# group = "control" if assgnt <= 1/K else "treatment"

##### Power analysis #####

### Statistical power analysis

effect_size = 2*asin(sqrt(0.194))-2*asin(sqrt(0.184))
ssp.tt_ind_solve_power(effect_size = effect_size, 
                       alpha = 0.05, 
                       nobs1 = None, 
                       alternative = 'larger', 
                       power=0.8)

### Single simulation for false positives (sample size = 2000)
random.seed(123)
np.random.seed(123)

# Simulating the experiment once with no true effect
def false_positive_single_sim(s = 2000): 
    
    sim_data_df = hist_data_df.sample(s)
    sim_data_df['prob_button'] = np.random.uniform(size=s)
    sim_data_df['oneclick'] = sim_data_df['prob_button'].apply(lambda x: 1 
                                                               if x > 0.5 
                                                               else 0)
    
    # Running a logistic regression
    model = smf.logit('booked ~ age + gender + period + oneclick', 
                      data = sim_data_df)
    res = model.fit(disp=0)
    res.summary()
    
    # Extracting the coefficients of interest from the summary
    coeff = res.params['oneclick']
    pvalue = res.pvalues['oneclick']
    
    return [coeff, pvalue]
false_positive_single_sim()

### Repeated simulation for false positives (no true effect)

nloops = 1000
sample_sizes = [2000] * nloops


def false_positive_repeat_sim(sample_sizes):
    #Serialized function with FOR loop
    coeff_vector = []
    pvalue_vector = []
    
    for s in sample_sizes:
        sim_data_df = hist_data_df.sample(s)
        sim_data_df['prob_button'] = np.random.uniform(size=s)
        sim_data_df['oneclick'] = sim_data_df['prob_button'].apply(lambda x: 1 
                                                                   if x > 0.5 
                                                                   else 0)
        
        model = smf.logit('booked ~ age + gender + period + oneclick', 
                          data = sim_data_df)
        res = model.fit(disp=0)
    
        # Extracting the coefficients of interest from the summary
        coeff_vector.append(res.params['oneclick'])
        pvalue_vector.append(res.pvalues['oneclick'])
    
    sim_summary_df = pd.DataFrame({'coeff': coeff_vector,
                                   'pvalue': pvalue_vector})
    return sim_summary_df                                       
sim_summary_df = false_positive_repeat_sim(sample_sizes)
sim_summary_df.describe()

#Alternative parallelized function for higher speed
def false_positive_repeat_sim_parallel(sample_sizes):
    #Parallelized version with joblib
    n_cpu = psutil.cpu_count() #Counting number of cores on machine
    temp_res = Parallel(n_jobs = n_cpu)(delayed(false_positive_single_sim)(n) 
                                    for n in sample_sizes)
    res_parallel = pd.DataFrame(temp_res, columns=['coeff', 'pvalue'])
    return res_parallel

sim_summary_df = false_positive_repeat_sim_parallel(sample_sizes)
sim_summary_df.describe()

#Calculating summary statistics on results of simulation
def f(c):
    return pd.Series([c.abs().min(), c.abs().mean(), c.abs().max()], 
                     index = ['min_eff', 'avg_eff', 'max_eff'])
sim_summary_df.loc[sim_summary_df.pvalue <=0.1].apply(f).drop('pvalue', axis=1)


### Repeated simulation for false negatives (with true effect)

nloops = 128
sample_sizes = [2000] * nloops

#Adding predicted probability of booking to historical data
hist_model = smf.logit('booked ~ age + gender + period', data = hist_data_df)
res = hist_model.fit()
hist_data_df['pred_prob_bkg'] = res.predict()
del res, hist_model

# Simulating the experiment once
def false_negative_single_sim(s=2000):
    
    #Setting the true effect size
    eff_size = 0.01
    #Assigning button to 50% of sample
    sim_data_df = hist_data_df.sample(s)
    sim_data_df['prob_button'] = np.random.uniform(size=s)
    sim_data_df['oneclick'] = sim_data_df['prob_button'].apply(lambda x: 1 
                                                               if x > 0.5 
                                                               else 0)
    #Updating probability of booking for people with button
    sim_data_df['pred_prob_bkg'] = np.where(sim_data_df['oneclick'] == 1, 
                                            sim_data_df['pred_prob_bkg'] 
                                            + eff_size, 
                                            sim_data_df['pred_prob_bkg'])
    #Generating binary value for booked based on new probability
    sim_data_df['prob_booked'] = np.random.uniform(size=s)
    sim_data_df['booked'] = np.where(sim_data_df['pred_prob_bkg'] 
                                     >= sim_data_df['prob_booked'], 1, 0)

    #Running new logistic regression, now including the one-click button
    model = smf.logit('booked ~ age + gender + period + oneclick', 
                      data = sim_data_df)
    res = model.fit(disp=0)
    res.summary()
    
    # Extracting the coefficients of interest from the summary
    coeff = res.params['oneclick']
    pvalue = res.pvalues['oneclick']
    
    return [coeff, pvalue]
false_negative_single_sim(s=2000)    

def false_negative_repeat_sim(sample_sizes):
    #Serialized function with FOR loop
    
    coeff_vector = []
    pvalue_vector = []
    eff_size = 0.01
    #Simulation loop    
    random.seed(123)
    np.random.seed(123)
    for s in sample_sizes:
        #Assigning button to 50% of sample
        sim_data_df = hist_data_df.sample(s)
        sim_data_df['prob_button'] = np.random.uniform(size=s)
        sim_data_df['oneclick'] = sim_data_df['prob_button'].apply(lambda x: 1 
                                                                   if x > 0.5 
                                                                   else 0)
        #Updating probability of booking for people with button
        sim_data_df['pred_prob_bkg'] = np.where(sim_data_df['oneclick'] == 1, 
                                                sim_data_df['pred_prob_bkg'] 
                                                + eff_size, 
                                                sim_data_df['pred_prob_bkg'])
        #Generating binary value for booked based on new probability
        sim_data_df['prob_booked'] = np.random.uniform(size=s)
        sim_data_df['booked'] = np.where(sim_data_df['pred_prob_bkg'] 
                                         >= sim_data_df['prob_booked'], 1, 0)
    
    
        model = smf.logit('booked ~ age + gender + period + oneclick', 
                          data = sim_data_df)
        res = model.fit(disp=0)
    
        # Extracting the coefficients of interest from the summary
        coeff_vector.append(res.params['oneclick'])
        pvalue_vector.append(res.pvalues['oneclick'])
    
    sim_summary_df = pd.DataFrame({'coeff': coeff_vector,
                                       'pvalue': pvalue_vector})
    return sim_summary_df

def false_negative_repeat_sim_parallel(sample_sizes):
    #Parallelized version with joblib
    n_cpu = psutil.cpu_count() #Counting number of cores on machine
    temp_res = Parallel(n_jobs = n_cpu)(delayed(false_negative_single_sim)(n) 
                                    for n in sample_sizes)
    res_parallel = pd.DataFrame(temp_res, columns=['coeff', 'pvalue'])
    return res_parallel

#Picking the results from whichever simulation was run 
sim_summary_df = false_negative_repeat_sim_parallel(sample_sizes)

#Getting the overall empirical power of our analysis
(len(sim_summary_df[(sim_summary_df['pvalue'] <= 0.1) 
                   & (sim_summary_df['coeff'] > 0)].index) 
 / len(sim_summary_df.index))

### Statistical significance and power simulation at scale

#Adding predicted probability of booking to historical data (if not done before)
hist_model = smf.logit('booked ~ age + gender + period', data = hist_data_df)
res = hist_model.fit()
hist_data_df['pred_prob_bkg'] = res.predict()
del res, hist_model

#Generating a list of sample sizes
nloops_per_ss = 200
ss_base = range(10000,60000,10000)
temp_list = [[s] * nloops_per_ss for s in ss_base]
sample_sizes = [item for sublist in temp_list for item in sublist]

#Power simulation function

def power_sim(sample_sizes, eff_size = 0.01):
    
    #Initializing the vectors where data will be temporarily stored
    coeff0 = []
    pvalue0 = []
    bkg_rate_ctrl0 = []
    bkg_rate_treat0 = [] 
    bkg_rate_diff0 = []
    coeff1 = []
    pvalue1 = []
    bkg_rate_ctrl1 = []
    bkg_rate_treat1 = [] 
    bkg_rate_diff1 = []
    test0_pvalue = []
    test1_pvalue = []
    
    for s in sample_sizes:
        
        #Assigning button to 50% of sample
        sim_data_df = hist_data_df.sample(s)
        sim_data_df['prob_button'] = np.random.uniform(size=s)
        sim_data_df['oneclick'] = sim_data_df['prob_button'].apply(lambda x: 1 
                                                                   if x > 0.5 
                                                                   else 0)
        
        ## Calculating statistical significance (false positives) variables
        
        #Saving booking rates in control and treatment groups
        bkg_rate_ctrl0.append(sim_data_df.loc[(sim_data_df.oneclick==0),
                                              'booked'].mean())
        bkg_rate_treat0.append(sim_data_df.loc[(sim_data_df.oneclick==1),
                                               'booked'].mean())
        
        #Saving differences in booking rates between the two groups
        bkg_rate_diff0.append(bkg_rate_treat0[-1] - bkg_rate_ctrl0[-1])
        
        #Test of proportion with no effect
        tab0 = sim_data_df.groupby('oneclick').agg(count_booked = ('booked',sum), 
                                          count_total = ('age','count'))
        test0 = sspr.proportions_ztest(count=tab0.count_booked, 
                                      nobs=tab0.count_total, 
                                      alternative='smaller')
        test0_pvalue.append(test0[1])
        
        #Logistic regression with no effect
        model0 = smf.logit('booked ~ age + gender + period + oneclick', 
                          data = sim_data_df)
        res0 = model0.fit(disp=0)
    
        # Extracting the coefficients of interest from the summary
        coeff0.append(res0.params['oneclick'])
        pvalue0.append(res0.pvalues['oneclick'])
        
        
        ## Calculating statistical power (false negatives) variables
        
        #Updating probability of booking for people with button
        sim_data_df['pred_prob_bkg'] = np.where(sim_data_df['oneclick'] == 1, 
                                                sim_data_df['pred_prob_bkg'] 
                                                + eff_size, 
                                                sim_data_df['pred_prob_bkg'])
        #Generating binary value for booked based on new probability
        sim_data_df['prob_booked'] = np.random.uniform(size=s)
        sim_data_df['booked'] = np.where(sim_data_df['pred_prob_bkg'] 
                                         >= sim_data_df['prob_booked'], 1, 0)
        
        #Saving booking rates in control and treatment groups
        bkg_rate_ctrl1.append(sim_data_df.loc[(sim_data_df.oneclick==0),
                                              'booked'].mean())
        bkg_rate_treat1.append(sim_data_df.loc[(sim_data_df.oneclick==1),
                                               'booked'].mean())
        
        #Saving differences in booking rates between the two groups
        bkg_rate_diff1.append(bkg_rate_treat1[-1] - bkg_rate_ctrl1[-1])
        
        #Test of proportion with true effect
        tab1 = sim_data_df.groupby('oneclick').agg(count_booked = ('booked',sum), 
                                          count_total = ('age','count'))
        test1 = sspr.proportions_ztest(count=tab1.count_booked, 
                                      nobs=tab1.count_total, 
                                      alternative='smaller')
        test1_pvalue.append(test1[1])
        
        #Logistic regression with true effect
        model1 = smf.logit('booked ~ age + gender + period + oneclick', 
                          data = sim_data_df)
        res1 = model1.fit(disp=0)
    
        # Extracting the coefficients of interest from the summary
        coeff1.append(res1.params['oneclick'])
        pvalue1.append(res1.pvalues['oneclick'])
        
    sim_summary_df = pd.DataFrame({'sample_size': sample_sizes,
                                   'coeff0': coeff0,
                                   'pvalue0': pvalue0,
                                   'bkg_rate_ctrl0': bkg_rate_ctrl0,
                                   'bkg_rate_treat0': bkg_rate_treat0,
                                   'bkg_rate_diff0': bkg_rate_diff0,
                                   'coeff1': coeff1,
                                   'pvalue1':pvalue1,
                                   'bkg_rate_ctrl1': bkg_rate_ctrl1,
                                   'bkg_rate_treat1': bkg_rate_treat1,
                                   'bkg_rate_diff1': bkg_rate_diff1,
                                   'test0_pvalue': test0_pvalue,
                                   'test1_pvalue': test1_pvalue
                                   })
    
    return sim_summary_df

sim_summary_df = power_sim(sample_sizes, eff_size = 0.01)
sim_summary_df.describe()

#Determining empirical power of test of proportions
sim_summary_df['pw_t1'] = np.where((sim_summary_df['test1_pvalue'] < 0.1) &
                                       (sim_summary_df['bkg_rate_diff1'] > 0), 1, 0)
test_results1 = sim_summary_df.groupby('sample_size').agg(count_TP=('pw_t1',sum),
                                          count_total = ('pw_t1', 'count'))
test_results1['power_test_prop'] = test_results1['count_TP'] / test_results1['count_total']
test_results1 = test_results1.drop(['count_TP','count_total'], axis=1)
test_results1

sns.distplot(sim_summary_df['test1_pvalue'])
sns.distplot(sim_summary_df['bkg_rate_diff1'])
sns.scatterplot(x='bkg_rate_diff1', y='test1_pvalue', data=sim_summary_df)
sns.scatterplot(x=[10,20,30,40,50], y='power_test_prop', data=test_results1)

#Determining empirical significance of test of proportions
sim_summary_df['pw_t0'] = np.where((sim_summary_df['test0_pvalue'] < 0.1), 1, 0)
test_results0 = sim_summary_df.groupby('sample_size').agg(count_FP=('pw_t0',sum),
                                          count_total = ('pw_t0', 'count'))
test_results0['stat_sig_test_prop'] = test_results0['count_FP'] / test_results0['count_total']
test_results0 = test_results0.drop(['count_FP','count_total'], axis=1)
test_results0

sns.distplot(sim_summary_df['test0_pvalue'])


#Determining empirical power of regression
sim_summary_df['pw_reg1'] = np.where((sim_summary_df['pvalue1'] < 0.1) &
                                       (sim_summary_df['coeff1'] > 0), 1, 0)
reg_results1 = sim_summary_df.groupby('sample_size').agg(count_TP=('pw_reg1',sum),
                                          count_total = ('pw_reg1', 'count'))
reg_results1['power_reg'] = reg_results1['count_TP'] / reg_results1['count_total']
reg_results1 = reg_results1.drop(['count_TP','count_total'], axis=1)
reg_results1

sns.scatterplot(x='coeff1', y='pvalue1', data=sim_summary_df)
sns.scatterplot(x=[10,20,30,40,50], y='power_reg', data=reg_results1)

#Determining empirical significance of regression
sim_summary_df['pw_reg0'] = np.where((sim_summary_df['pvalue0'] < 0.1), 1, 0)
reg_results0 = sim_summary_df.groupby('sample_size').agg(count_FP=('pw_reg0',sum),
                                          count_total = ('pw_reg0', 'count'))
reg_results0['stat_sig_reg'] = reg_results0['count_FP'] / reg_results0['count_total']
reg_results0 = reg_results0.drop(['count_FP','count_total'], axis=1)
reg_results0

sns.scatterplot(x='coeff0', y='pvalue0', data=sim_summary_df)

##### Analyzing the results of the experiment #####

### Test of proportions on booking probability

exp_data_df.groupby('oneclick').agg(booked_percentage = ('booked',np.mean))
tab = exp_data_df.groupby('oneclick').agg(count_booked = ('booked',sum), 
                                          count_total = ('age','count'))
sspr.proportions_ztest(count=tab.count_booked, 
                       nobs=tab.count_total, 
                       alternative='larger')

### Logistic regression
import statsmodels.formula.api as smf
model = smf.logit('booked ~ age + gender + oneclick', data = exp_data_df)
res = model.fit()
res.summary()

### Calculating average difference in probabilities

#Creating new copies of data
no_button_df = exp_data_df[['age', 'gender']]
no_button_df['oneclick'] = 0
button_df = exp_data_df[['age', 'gender']]
button_df['oneclick'] = 1

#Adding the predictions of the model 
no_button_df['pred_bkg_rate'] = res.predict(no_button_df)
button_df['pred_bkg_rate'] = res.predict(button_df)

diff = button_df['pred_bkg_rate'] - no_button_df['pred_bkg_rate']
diff.mean()