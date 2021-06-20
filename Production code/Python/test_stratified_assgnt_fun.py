# -*- coding: utf-8 -*-
"""
Tests for strat_prep_fun function
"""
# Common packages
import pandas as pd
import numpy as np
import unittest

from functions import stratified_assgnt_fun



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

class TestStratifiedAssgntFun(unittest.TestCase):
    
    # function handles issues with number of rows
    def test_number_rows(self):
        pass
            
    # function output is correctly formatted
    def test_output_formatting(self):
        simplest_output = stratified_assgnt_fun(simplest_input, id_var)
        self.assertIsInstance(simplest_output, pd.DataFrame)
        self.assertEqual(len(simplest_output), len(simplest_input))
        self.assertTrue((simplest_output.loc[:,id_var] == simplest_input.loc[:,id_var]).all())
        
    def test_expected_simple_outcome(self):
        simplest_output = stratified_assgnt_fun(simplest_input, id_var)
        self.assertTrue(simplest_output.loc[simplest_output['name'] == 'Anna','group'].tolist() !=\
                         simplest_output.loc[simplest_output['name'] == 'Barbara','group'].tolist())
        self.assertTrue(simplest_output.loc[simplest_output['name'] == 'Amalric','group'].tolist() !=\
                         simplest_output.loc[simplest_output['name'] == 'Bob','group'].tolist())
        
        
if __name__ == '__main__':
    unittest.main()