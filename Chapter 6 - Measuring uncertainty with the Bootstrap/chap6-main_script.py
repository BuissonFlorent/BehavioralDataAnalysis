# -*- coding: utf-8 -*-
"""
Created on Sun May 10 11:44:30 2020

@author: Florent
"""

##### Data and libraries

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
from statsmodels.formula.api import ols
import statsmodels.stats.outliers_influence as st_inf # For Cook's distance

### Generating the data

times = [2,2,3,5,6,9,10,47,61,413]
experience = [11,17,18,1,10,4,6,3,8,0]

##### Intro to the Bootstrap #####

data_df = pd.DataFrame(
    {'times': times, 
    'experience': experience})

# Building linear model
lin_mod = ols("times~1", data=data_df).fit()
print(lin_mod.summary())

est = lin_mod.params['Intercept']
se = lin_mod.bse['Intercept']

#Building normal confidence interval
LL = est-1.96*se #Lower limit
UL = est+1.96*se #Upper limit
print("LL = ", LL)
print("UL = ",UL)


#Building bootstrap CI
mean_lst = []
B = 2000
N = len(data_df)
for i in range(B):   
    boot_df = data_df.sample(N, replace = True)
    M = np.mean(boot_df.times)
    mean_lst.append(M)

LL_b = np.quantile(mean_lst, 0.025)  
UL_b = np.quantile(mean_lst, 0.975)
print("LL_b = ", LL_b)
print("UL_b = ",UL_b)
                   
#### Bootstrap for time promise
promise_lst = []
B = 2000
N = len(data_df)
for i in range(B):
    boot_df = data_df.sample(N, replace = True)
    above180 =  len(boot_df[boot_df.times >= 180]) / N
    promise_lst.append(above180)
LL_b = np.quantile(promise_lst, 0.025)  
UL_b = np.quantile(promise_lst, 0.975)  

##### Bootstrap for regression analysis #####

print(ols("times~experience", data=data_df).fit().summary())

reg_lst = []
B = 4000
N = len(data_df)
for i in range(B):
    boot_df = data_df.sample(N, replace = True)
    lin_mod = ols("times~experience", data=boot_df).fit()
    coeff = lin_mod.params['experience']
    reg_lst.append(coeff)
LL_b = np.quantile(reg_lst, 0.025)  
UL_b = np.quantile(reg_lst, 0.975) 

sns.distplot(reg_lst) 

pval = 2 * sum(1 for x in reg_lst if x > 0) / B

##### When to use the Bootstrap #####

lin_mod = ols("times~experience", data=data_df).fit()

#Extract Cook's distance for influential points
CD = st_inf.OLSInfluence(lin_mod).summary_frame()['cooks_d']
CD[CD > 1]

#Density and QQ plot of residuals
res_df = lin_mod.resid

sns.kdeplot(res_df)

fig = sm.qqplot(res_df, line='s')
plt.show()

##### Optimizing the Bootstrap in R and Python #####
    
#Creating unique numpy array for sampling
data_ar = data_df.to_numpy()
rng = np.random.default_rng()

np_lst = []
for i in range(B): 
    
    #Extracting the relevant columns from array
    boot_ar = rng.choice(data_ar, size=N, replace=True)
    X = boot_ar[:,1]
    X = np.c_[X, np.ones(N)]
    Y = boot_ar[:,0]
    
    ### LSTQ implementation
    np_lst.append(np.linalg.lstsq(X, Y, rcond=-1)[0][0])

#Plotting histogram
sns.distplot(np_lst)

LL_b_np = np.quantile(np_lst, 0.025)  
UL_b_np = np.quantile(np_lst, 0.975) 