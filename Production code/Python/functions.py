# -*- coding: utf-8 -*-
"""
Production code for stratified randomization functions and Cramer's V
"""

# Common packages
import pandas as pd
import numpy as np
import warnings

# To rescale numeric variables
from sklearn.preprocessing import MinMaxScaler
# To one-hot encode cat. variables
from sklearn.preprocessing import OneHotEncoder
# For Cramer's V
from scipy.stats import chi2_contingency 
from math import sqrt

## Python code (output not shown)
def strat_prep_fun(dat_df, id_var):
    
    #Isolating the identification variable
    assert id_var in dat_df.columns,\
        "the id_var string doesn't match any column name"
    dat_out_np = np.array(dat_df.loc[:,id_var].values.tolist())
    dat_out_np = np.reshape(dat_out_np, (len(dat_out_np), 1))
    dat_df = dat_df.drop([id_var], axis=1)
    
    #Input validation
    assert dat_df.select_dtypes(exclude = ['int64', 'float64', 'object']).empty,\
        "please format all data columns to numeric, integer or character (for categorical variables)"
    
    ## Handling categorical variables
    cat_df = dat_df.copy().select_dtypes(include = 'object') #Categorical vars
    if not cat_df.empty:
        # One-hot encoding all categorical variables
        enc = OneHotEncoder(handle_unknown='ignore')
        enc.fit(cat_df)
        cat_np = enc.transform(cat_df).toarray()
        dat_out_np = np.concatenate((dat_out_np, cat_np), axis=1)
        
    ## Handling numerical variables
    num_df = dat_df.copy().select_dtypes(include = ['int64', 'float64']) #Numeric vars
    if not num_df.empty:
        # Normalizing all numeric variables to [0,1]
        scaler = MinMaxScaler()
        scaler.fit(num_df)
        num_np = scaler.transform(num_df)
        dat_out_np = np.concatenate((dat_out_np, num_np), axis=1)
    
    return dat_out_np

def stratified_assgnt_fun(dat_df, id_var, n_groups = 2, group_var_name = "group"):
    
    #Prepping the data
    data_np = strat_prep_fun(dat_df, id_var)
    
    #Isolating the identification variable
    dat_ID = data_np[:,0].tolist() # Extract ID for later join
    data_np = data_np[:,1:].astype(np.float)
    
    ## Matching algorithm
    
    #Setup
    N = len(data_np)
    match_len = n_groups - 1 # Number of matches we want to find
    match_idx = match_len - 1 # Accounting for 0-indexing
    
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
    exp_grps = np.array(list(range(n_groups))*(int(N/n_groups))).reshape((int(N/n_groups),n_groups))
    exp_grps = exp_grps.tolist()
    for j in exp_grps: 
        np.random.shuffle(j)
    #flattening the two lists
    import itertools
    exp_grps = list(itertools.chain(*exp_grps))
    matches_lst2 = list(itertools.chain(*matches_lst))
    exp_grps2 = [x for _,x in sorted(zip(matches_lst2,exp_grps))]
    
    assgnt_df = pd.DataFrame(exp_grps2, columns=[group_var_name])
    assgnt_df[group_var_name] = assgnt_df[group_var_name].astype(str)
    assgnt_df[id_var] = dat_ID
    dat_df = dat_df.merge(assgnt_df, on=id_var, how='inner')
    return dat_df

def cramer_v(var1, var2):
    
    #Validating input
    n = len(var1)
    m = len(var2) #For validation purposes only
    assert n > 1, "The data has only one row"
    assert n == m, "The two variables don't have the same number of rows."
    
    
    k = len(var1.unique())
    r = len(var2.unique())
    if k == 2 and r == 2:
        warnings.warn("The two variables are binary, maximum correlation is lower than 1.", UserWarning)
        print("WARNING: the two variables are binary, maximum correlation is lower than 1.")
        
    pivot_tb = pd.crosstab(var1, var2, margins=False)
    chi_sq, _, _, _ = chi2_contingency(pivot_tb) 
    V = sqrt((chi_sq/n)/(min(k-1, r-1)))
    return V