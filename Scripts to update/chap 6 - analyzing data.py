# -*- coding: utf-8 -*-
"""
Created on Sat Jun 13 10:07:24 2020

@author: Florent
"""

#################################
##### This script analyzes the data used in chapter 6, 
##### Handling Missing Data
#################################

##### Data and libraries #####

# Common libraries
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
from statsmodels.formula.api import ols
import matplotlib.pyplot as plt
import seaborn as sns

# Chapter-specific libraries
from statsmodels.imputation import mice
import statsmodels.api as sm # For OLS call in Mice

#Loading the data
complete_data_df = pd.read_csv("chap6-complete_data.csv")
available_data_df = pd.read_csv("chap6-available_data.csv")
available_data_supp_df = pd.read_csv("chap6-available_data_supp.csv")

#Reformatting factor variables
complete_data_df['gender'] = pd.Categorical(complete_data_df.gender, 
                                      categories=['M','F'])
available_data_df['gender'] = pd.Categorical(available_data_df.gender, 
                                      categories=['M','F'])
complete_data_df['state'] = pd.Categorical(complete_data_df.state,
                                            categories=['A','B','C'])
available_data_df['state'] = pd.Categorical(available_data_df.state,
                                            categories=['A','B','C'])

##### Visualizing patterns of missingness #####

### Visualizing missing data "a la" VIM

#Identifying columns with any missing data
all_cols = available_data_df.columns.tolist()
miss_cols = [col for col in all_cols if available_data_df[col].isnull().sum()]
if miss_cols == all_cols: available_data_df['index'] = available_data_df.index
print(available_data_df.isnull().groupby(miss_cols).count())
print(available_data_df.isnull().sum())
    
    
#Quantifying the amount of missing data for a variable   
min_data_df = available_data_df.copy()
min_data_df.neuro = np.where(min_data_df.neuro.isna(), min_data_df.neuro.min(), 
                             min_data_df.neuro)

max_data_df = available_data_df.copy()
max_data_df.neuro = np.where(max_data_df.neuro.isna(), max_data_df.neuro.max(), 
                             max_data_df.neuro)


print(ols("bkg_amt~neuro", data=min_data_df).fit().summary())
print(ols("bkg_amt~neuro", data=max_data_df).fit().summary())


##### 2. Level of missingness - Tacoma/Tampa illustration #####
   
#Loading the data
tacoma_df = pd.read_csv("chap6-tacoma.csv")
tampa_df = pd.read_csv("chap6-tampa.csv")
    
# Building the correlation matrices
tampa_miss_df = tampa_df.copy().drop(['ID'], axis=1).isna()
tacoma_miss_df = tacoma_df.copy().drop(['ID'], axis=1).isna()

tampa_cor = tampa_miss_df.corr()
tacoma_cor = tacoma_miss_df.corr()

sn.heatmap(tampa_cor, annot=True, vmin=-0.05, vmax=1, cmap="YlGnBu")
plt.show()
sn.heatmap(tacoma_cor, annot=True, vmin=-0.05, vmax=1, cmap="YlGnBu")
plt.show()

#Sources of missingness for extraversion
available_data_df['md_extra'] = available_data_df['extra'].isnull().astype(float)
md_extra_mod =smf.logit('md_extra~age+open+neuro+gender+state+bkg_amt',
                      data=available_data_df)
md_extra_mod.fit().summary()

#Sources of missingness for state
available_data_df['md_state'] = available_data_df['state'].isnull()\
    .astype(float)
md_state_mod =smf.logit('md_state~age+open+extra+neuro+gender+bkg_amt',
                      data=available_data_df)
md_state_mod.fit(disp=0).summary()


##### MICE imputation and analysis #####

#One-hot encoding categorical variables
gender_dummies = pd.get_dummies(available_data_df.gender, prefix='gender')
state_dummies = pd.get_dummies(available_data_df.state, prefix='state')
available_data_df =  pd.concat([available_data_df, 
                                gender_dummies, state_dummies], 
                               axis=1)
available_data_df.gender_F = np.where(available_data_df.gender.isna(), 
                                      float('NaN'), available_data_df.gender_F)
available_data_df.gender_M = np.where(available_data_df.gender.isna(), 
                                      float('NaN'), available_data_df.gender_M)
available_data_df.state_A = np.where(available_data_df.state.isna(), 
                                     float('NaN'), available_data_df.state_A)
available_data_df.state_B = np.where(available_data_df.state.isna(), 
                                     float('NaN'), available_data_df.state_B)
available_data_df.state_C = np.where(available_data_df.state.isna(), 
                                     float('NaN'), available_data_df.state_C)
available_data_df =  available_data_df.drop(['gender', 'state'], axis=1)

#MICE imputation
MI_data_df = mice.MICEData(available_data_df)                                 
fit = mice.MICE(model_formula='bkg_amt ~ age + open + extra + neuro + \
                gender_M + gender_F + state_A + state_B + state_C', 
                model_class=sm.OLS, data=MI_data_df)                
MI_summ = fit.fit(n_imputations=20).summary()  
print(MI_summ)

#Adding auxiliary variables
augmented_data_df = pd.concat([available_data_df, available_data_supp_df], 
                              axis=1)
MI_data_aux_df = mice.MICEData(augmented_data_df)   