# -*- coding: utf-8 -*-
"""manipulate_data.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1gD5oW07G-nMEbf8RjtxmzdRuCBb6XslN

The main goal of this notebook is to provide useful functions for managing the data.

## Libraries imports
"""

import pandas as pd
import numpy as np

import matplotlib.pyplot as plt

from sklearn.preprocessing import StandardScaler # To standardize the data

"""## Useful functions"""

def get_temporal_variables(data):
  temporal_variables = {}
  waves_columns = [col for col in data.columns if "genetic_" not in col and col[1] in "123456789"]
  for col in waves_columns:
    char = col[0] # R or H
    if col[2] in "01234":
      wave = col[1:3]
      suffix = col[3:]
    else:
      wave = col[1]
      suffix = col[2:]
    variable = char + 'w' + suffix
    
    if variable not in temporal_variables.keys():
      temporal_variables[variable] = np.zeros((14), dtype=bool)
    
    temporal_variables[variable][int(wave)-1] = True

  temporal_variables = pd.DataFrame(temporal_variables)

  # We manually add "GHIw":
  temporal_variables["GHIw"] = np.ones((14), dtype=bool)
  waves_columns += [f"GHI{w}" for w in range(1,15)]

  return temporal_variables

def get_sample(data, waves=[1,2,3,4,5,6,7,8,9,10,11,12,13,14], keep_genetic=True, keep_GHI=True, keep_other_ID=False):
  ''' 
  Returns the part of the data corresponding to given waves. Only individuals
  present in the specified waves are kept.

  Set keep_genetic = False if you don't want your sample to contain genetic data
  Set keep_GHI = False if you don't want your sample to contain the global health
  Set keep_other_ID = True if you want to keep ID columns different from the main
  HHIDPN (for example, HwHHID or HHID or PN)
  index (the tSNE-GHI).
  '''

  # Get only people present in specified waves
  criteria = 1
  for wave in waves:
    criteria = criteria * data[f"INW{wave}"]

  # Get only variables relative to specified waves
  temporal_variables = get_temporal_variables(data)
  
  columns_to_keep = ["HHIDPN"]
  for wave in waves:
    are_variables_in_wave = temporal_variables.iloc[wave-1]
    wave_available_variables = are_variables_in_wave.index[are_variables_in_wave]
    wave_variables = [var.replace('w', str(wave)) for var in wave_available_variables]

    columns_to_keep += wave_variables
  
  if keep_genetic:
    genetic_columns = [col for col in data.columns if "genetic_" in col]
    columns_to_keep += genetic_columns

  if not keep_GHI:
    for wave in waves:
      columns_to_keep.remove(f"GHI{wave}")

  if not keep_other_ID:
    for wave in waves:
      columns_to_keep.remove(f"H{wave}HHIDC")
      columns_to_keep.remove(f"H{wave}HHID")
    
  
  return data.loc[criteria == 1, columns_to_keep].reset_index(drop=True)

def get_columns_types(data, columns_types):
  """
  Returns a dictionary which describes all categorical and continuous variables.

  ------------
  Example:
  data.columns = ["Age", "Sexe", "Weight"]
  
  return: {"Categ" : ["Sexe"], "Cont" : ["Age", Weight]}
  """

  data_columns_types = pd.DataFrame(data.columns, columns=["Name"]).merge(columns_types, on= "Name", how="left")

  return data_columns_types.groupby(["Type"])["Name"].apply(list).to_dict()

if __name__ == "__main__":
  columns_types = pd.read_csv("/content/drive/MyDrive/Statapp/data_03_columns_types.csv")
  data = pd.read_csv("/content/drive/MyDrive/Statapp/data_03.csv")