# -*- coding: utf-8 -*-
"""
Created on Wed Jun 16 08:23:39 2021

@author: Florent
"""

import pandas as pd
import numpy as np
import unittest
from functions import cramer_v


df_input = pd.DataFrame({
  'name': ["Anna", "Amalric", "Arya", "Barbara", "Bob", "Ben"],
  'color': ["blue", "red", "green", "yellow", "blue", "purple"],
  'flavor': ["chocolate", "vanilla", "vanilla", "chocolate", "vanilla", "chocolate"]
})

color = df_input.loc[:,'color']
color_short = color[0:-1]

flavor = df_input.loc[:,'flavor']




class TestCramerV(unittest.TestCase):
        
    def test_input_validation(self):
        #The two variables don't have the same length
        with self.assertRaises(AssertionError):
            cramer_v(color, color_short)
        #One of the variables has a length of 1
        with self.assertRaises(AssertionError):
            cramer_v(color[0], color_short)
        #One of the variables is not categorical
        
            
    def test_expected_simple_outcome(self):
        out = cramer_v(color, color)
        self.assertEqual(out, 1)
        
if __name__ == '__main__':
    unittest.main()







