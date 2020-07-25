# -*- coding: utf-8 -*-
"""
Created on Sat Jul 25 09:10:19 2020

@author: Florent
"""

import os
import pandas as pd
from statsmodels.formula.api import ols
import matplotlib.pyplot as plt
import seaborn as sns


os.chdir('C:\\Users\\Florent\\Dropbox\\Synchronised\\Work_and_projects\\Behavioral data science book\\R scripts\\transfered to Github')


##### First example: stand data #####

#Reading the data
stand_data_df = pd.read_csv('chap2-stand_data.csv')

#Plotting ice-cream sales against temperatures
sns.scatterplot(x='temps', y='icecream_sales', data=stand_data_df)

#Running linear regressions 
#Biased model (coeff should be 1,000)
model1 = ols("icecream_sales ~ temps", data=stand_data_df)
print(model1.fit().summary())

#correct model for icecream (coeffs should be 1,000 and 20,000)
model2 = ols("icecream_sales ~ temps + summer_months", data=stand_data_df)
print(model2.fit().summary())

#Model biased by extra controlling
model3 = ols("icecream_sales ~ temps + summer_months + iced_coffee_sales", 
             data=stand_data_df)
print(model3.fit().summary())



##### Second example: survey data #####

#Reading the data
survey_data_df = pd.read_csv('chap2-survey_data.csv')

#Reformatting shopped variable to binary
survey_data_df['shopped'] = pd.Categorical(survey_data_df.shopped, 
                                      categories=[0,1], 
                                      ordered = True)

### Plotting relationships between variables in the data

fig, (ax1, ax2, ax3) = plt.subplots(ncols=3)
#Scatterplot of chocolate versus vanilla taste 
ax1.set_xlim(0,28)
ax1.set_ylim(0,28)
ax1.set_xlabel('Taste for vanilla')
ax1.set_ylabel('Taste for chocolate')
sns.regplot(x='vanilla', y='chocolate', data=survey_data_df, 
            line_kws={'color':'black'}, ax=ax1)
#Boxplot of vanilla taste against shopping behavior
ax2.set_xlabel('Shopped (Y/N)')
ax2.set_ylabel('Taste for vanilla')
sns.boxplot(x='shopped', y='vanilla', data=survey_data_df, ax=ax2)
#Boxplot of chocolate taste against shopping behavior
ax3.set_xlabel('Shopped (Y/N)')
ax3.set_ylabel('Taste for chocolate')
sns.boxplot(x='shopped', y='chocolate', data=survey_data_df, ax=ax3)

fig.tight_layout()
plt.show()

### Plotting same scatterplot but for shoppers only

#Scatterplot of chocolate versus vanilla taste 
sns.regplot(x='vanilla', y='chocolate', 
            data=survey_data_df[survey_data_df.shopped==1], 
            line_kws={'color':'black'})


