import ot
import pandas as pd
import numpy as np
import matplotlib.pyplot as pl
import ot.plot

df_unaware = pd.read_csv('../data/factuals_unaware.csv')

x_S = df_unaware.drop(columns=['pred', 'type', 'id_indiv', 'S_origin', 'Y'])
x_S.head()

x_white = x_S[x_S['S'] == 'White']
x_white = x_white.drop(columns=['S'])
x_black = x_S[x_S['S'] == 'Black']
x_black = x_black.drop(columns=['S'])

n_white = len(x_white)
n_black = len(x_black)
# Uniform weights
w_white = (1/n_white)*np.ones(n_white)
w_black = (1/n_black)*np.ones(n_black)

x_white = x_white.to_numpy()
x_black = x_black.to_numpy()
C = ot.dist(x_white, x_black)

pl.figure(1)
pl.plot(x_white[:, 0], x_white[:, 1], '+b', label='Source samples')
pl.plot(x_black[:, 0], x_black[:, 1], 'xr', label='Target samples')
pl.legend(loc=0)
pl.title('Source and target distributions')

pl.figure(2)
pl.imshow(C, interpolation='nearest')
pl.title('Cost matrix C')

# The transport plan: White –> Black
pi_white_black = ot.emd(w_white, w_black, C, numItermax=1e8)
# The transport plan: Black -> White
pi_black_white = pi_white_black.T
pi_white_black.shape
sum_of_rows = np.sum(pi_white_black, axis=1)
sum_of_rows*n_white
pi_black_white.shape
sum_of_rows = np.sum(pi_black_white, axis=1)
sum_of_rows*n_black

pl.figure(3)
pl.imshow(pi_white_black, interpolation='nearest')
pl.title('OT matrix pi_white_black')

pl.figure(4)
ot.plot.plot2D_samples_mat(x_white, x_black, pi_white_black, c=[.5, .5, 1])
pl.plot(x_white[:, 0], x_white[:, 1], '+b', label='Source samples')
pl.plot(x_black[:, 0], x_black[:, 1], 'xr', label='Target samples')
pl.legend(loc=0)
pl.title('OT matrix with samples')

# Counterfactuals for White individuals
transformed_x_white = n_white*pi_white_black@x_black
transformed_x_white.shape
transformed_x_white
# Counterfactuals for Black individuals
transformed_x_black = n_black*pi_black_white@x_white
transformed_x_black.shape
transformed_x_black

counterfactual_x = x_S.drop(columns=['S'])
counterfactual_x[x_S['S'] == 'White'] = transformed_x_white
counterfactual_x[x_S['S'] == 'Black'] = transformed_x_black
counterfactual_x.shape

# Export results
csv_file_path = '../data/counterfactuals_ot.csv'
counterfactual_x.to_csv(csv_file_path, index=False)
