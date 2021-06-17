# -*- coding: utf-8 -*-
"""
Tests for strat_prep_fun function
"""
# Common packages
import pandas as pd
import numpy as np
import unittest

from functions import strat_prep_fun



#Input datasets
simplest_input = pd.DataFrame({
  'name': ["Anna", "Amalric", "Barbara", "Bob"],
  'gender': ["F", "M", "F", "M"],
  'age': [40, 20, 41, 21]
}) 

character_input = pd.DataFrame({
  'name': ["Anna", "Amalric", "Barbara", "Bob"],
  'color': ["blue", "red", "yellow", "blue"]
})

factor_input = pd.DataFrame({
  'name': ["Anna", "Amalric", "Barbara", "Bob"],
  'color': ["blue", "red", "yellow", "blue"],
  'flavor': ["chocolate", "vanilla", "chocolate", "vanilla"]
})

numeric_input = pd.DataFrame({
  'name': ["Anna", "Amalric", "Barbara", "Bob"],
  'age': [40, 20, 41, 21],
  'height': [6, 5, 6.1, 5.1]
})

dat_df = simplest_input
id_var = 'name'
n_groups = 2
group_var_name = "group"

class TestStratPrepFun(unittest.TestCase):
    
    # function output is correctly formatted
    def test_output_formatting(self):
        simplest_output = strat_prep_fun(simplest_input, id_var)
        self.assertIsInstance(simplest_output, np.ndarray)
        self.assertEqual(len(simplest_output), len(simplest_input))
        self.assertEqual(simplest_output[:,0].tolist(), simplest_input.loc[:,id_var].tolist())
        
if __name__ == '__main__':
    unittest.main()