import ot
import pandas as pd
import numpy as np
import matplotlib.pyplot as pl
import ot.plot

# Read data
tb = pd.read_csv('../data/D_SXY_indep.csv')
x_S = tb.drop(columns=['Y'])
x_S.head()

# New point
new_point = pd.DataFrame({'S': [0], 'X1': [0], 'X2': [0.5]})

x_S = pd.concat([x_S, new_point], ignore_index=True)

x_0 = x_S[x_S['S'] == 0]
x_0 = x_0.drop(columns=['S'])
x_1 = x_S[x_S['S'] == 1]
x_1 = x_1.drop(columns=['S'])

n_0 = len(x_0)
n_1 = len(x_1)
# Uniform weights
w_0 = (1/n_0)*np.ones(n_0)
w_1 = (1/n_1)*np.ones(n_1)

# Cost matrix
x_0 = x_0.to_numpy()
x_1 = x_1.to_numpy()
C = ot.dist(x_0, x_1)

# Transport plan: from 0 to 1
pi_0_1 = ot.emd(w_0, w_1, C, numItermax=1e8)
pi_1_0 = pi_0_1.T
pi_0_1.shape

sum_of_rows = np.sum(pi_0_1, axis=1)
sum_of_rows*n_0
pi_1_0.shape
sum_of_rows = np.sum(pi_1_0, axis=1)
sum_of_rows*n_1

# Transported values
transformed_x_0 = n_0*pi_0_1@x_1
transformed_x_1 = n_1*pi_1_0@x_0

# Counterfactuals in a table
counterfactual_x = x_S.drop(columns=['S'])
counterfactual_x[x_S['S'] == 0] = transformed_x_0
counterfactual_x[x_S['S'] == 1] = transformed_x_1

# Export results
csv_file_path = '../data/counterfactuals_ot_test_indep.csv'
counterfactual_x.to_csv(csv_file_path, index=False)
