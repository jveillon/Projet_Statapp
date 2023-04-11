Statapp project.

Objective: Based on the socioeconomic and genetic data of an individual, is it possible to determine his global health?


Explanation of the project arborescence:

Repository 'pretreatment': 

- file_00: we merge the socioeconomic database and the genetic database into one dataset
- file_01: we merge section A and E in genetic data.
- file_02: we drop spouses'related columns and columns with more than 65% of missing values.
- file_03: we extract each column's type from the pdf documentation, and cast these types onto our data. We then transform categorical data into dummy variables.
- file_04: implementation of the 'Lasso with High Missing Rate', namely 'HMLasso'.
- file_05: application of the pooled HMLasso with X = data from wave 1 to 13, y = global health index. We proceed this way to further drop useless columns.
- file_06: we impute remaining missing values.