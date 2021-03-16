# -*- coding: utf-8 -*-
"""
Created on Sat Jun 13 10:07:24 2020

@author: Florent
"""

### Part II Chapter 3 : Building CDs from scratch
## chap 3 - analyzing example data.R
##
## This script loads the example data, based on data from Antonio, de Almeida & Nunes, 
## "Hotel booking demand datasets", Data in Brief, 2019.  https://doi.org/10.1016/j.dib.2018.11.126.
## and analyzes it

#Importing libraries
import pandas as pd
import numpy as np
from math import sqrt
from scipy.stats import chi2_contingency
import seaborn as sns
import matplotlib.pyplot as plt

#Loading the data
dat_df = pd.read_csv("chap3-hotel_booking_case_study.csv")

#Reducing the number of values for Country of origin by keeping most frequent countries only
#and aggregating the remaining ones under "Other"
countries_df = dat_df.groupby('Country').agg(pct = ('NRDeposit', lambda x: len(x)/len(dat_df))).\
sort_values(by=['pct'], ascending = False)
top_countries_df = countries_df.loc[countries_df.pct >= 0.01].reset_index()
top_countries_lst = top_countries_df['Country'].tolist()
print(top_countries_lst)
dat_df['Country'] = np.where(dat_df['Country'].isin(top_countries_lst), dat_df['Country'], 'Other')

#Cancellation rate by deposit types
table_cnt = dat_df.groupby(['NRDeposit', 'IsCanceled']).agg(cnt = ('Country', lambda x: len(x)))
print(table_cnt)

table_pct = table_cnt.groupby(level=0).apply(lambda x: 100 * x/float(x.sum()))
print(table_pct)

### Applying Cramer's V
#Formula from https://en.wikipedia.org/wiki/Cram%C3%A9r's_V 
def  CramerV(var1, var2):
    pivot_tb = pd.crosstab(var1, var2, margins=False)
    chi_sq, _, _, _ = chi2_contingency(pivot_tb) 
    n = len(var1)
    k = len(var1.unique())
    r = len(var2.unique())
    V = sqrt((chi_sq/n)/(min(k-1, r-1)))
    return V
V= CramerV(dat_df['NRDeposit'], dat_df['IsCanceled'])   
print(V)

#Shortening column names
dat_df.rename(columns=
              {"CustomerType": "CustTyp",
               "DistributionChannel": "DistCh",
               "IsRepeatedGuest": "RepGst",
               "MarketSegment": "MktSgmt",
               "IsCanceled": "IsCanc",
               "PreviousCancellations": "PrevCan",
               "NRDeposit": "NRDep"},
              inplace=True)
print(dat_df.columns)

#Creating a dataframe with the numerical and binary variables only
num_dat_df = dat_df.copy().select_dtypes(include=['int64', 'float64'])
num_dat_df.columns

#Creating the correlation matrix for numerical and binary variables only
num_corr = num_dat_df.corr()
print(num_corr)

f, ax = plt.subplots(figsize=(10, 8))
sns.heatmap(num_corr, mask=np.zeros_like(num_corr, dtype=np.bool), cmap=sns.diverging_palette(220, 10, as_cmap=True),
            square=True, ax=ax)

#Convert variables to categorical 
dat_df['NRDep'] = pd.Categorical(dat_df.NRDep)
dat_df['IsCanc'] = pd.Categorical(dat_df.IsCanc)
dat_df['DistCh'] = pd.Categorical(dat_df.DistCh)
dat_df['MktSgmt'] = pd.Categorical(dat_df.MktSgmt)
dat_df['CustTyp'] = pd.Categorical(dat_df.CustTyp)
dat_df['PrevCan'] = pd.Categorical(dat_df.PrevCan)
dat_df['RepGst'] = pd.Categorical(dat_df.RepGst)
dat_df['Country'] = pd.Categorical(dat_df.Country)
dat_df['Quarter'] = pd.Categorical(dat_df.Quarter)
dat_df.dtypes

# Calculating correlation matrix for the categorical and binary variables only
def cat_corr_fun(dat_df):
    
    #Initialize output dataframe
    cat_corr_df = pd.DataFrame(columns=['varI', 'varJ', 'corr'])
    
    #Subset to categorical variables only
    cat_dat_df = dat_df.select_dtypes(exclude=['int64', 'float64'])
    
    
    for varI in cat_dat_df.columns:
        for varJ in cat_dat_df.columns:
                corr = CramerV(cat_dat_df[varI], cat_dat_df[varJ])
                row = pd.Series(data={'varI':varI, 'varJ':varJ, 'corr':corr})
                cat_corr_df = cat_corr_df.append(row, ignore_index=True)
    return(cat_corr_df)
cat_corr_df = cat_corr_fun(dat_df)
cat_corr = pd.pivot_table(cat_corr_df, index=['varI'], columns=['varJ'], values=['corr'], fill_value=1)
cat_corr.columns = cat_corr.columns.droplevel(0)
print(cat_corr)
f, ax = plt.subplots(figsize=(10, 8))
sns.heatmap(cat_corr, mask=np.zeros_like(cat_corr, dtype=np.bool), cmap=sns.diverging_palette(220, 10, as_cmap=True),
            square=True, ax=ax)

##### Correlations between numeric and categorical variables #####
dat_df.groupby('CustTyp').agg(ADR = ('ADR', np.mean))

