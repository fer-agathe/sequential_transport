import ot
import pandas as pd
import numpy as np
import matplotlib.pyplot as pl
import ot.plot
from scipy.spatial.distance import cdist
import sklearn as sk

# Import data
df_aware = pd.read_csv('../data/factuals_aware-compas.csv')
x_S = df_aware.drop(columns=['race_origin', 'pred', 'type', 'id_indiv'])

# Split according to sensitive attribute
x_S_0 = x_S[x_S['race'] == 'Non-White']
x_S_1 = x_S[x_S['race'] == 'White']

x_S_0 = x_S_0.drop(columns=['race'])
x_S_1 = x_S_1.drop(columns=['race'])

n_0 = len(x_S_0)
n_1 = len(x_S_1)

# Separate numerical and categorical variables
num_cols = x_S_0.select_dtypes(include=[np.number]).columns
cat_cols = x_S_0.select_dtypes(include=[object, 'category']).columns
num_0, num_1 = x_S_0[num_cols], x_S_1[num_cols]
cat_0, cat_1 = x_S_0[cat_cols], x_S_1[cat_cols]

category_counts = x_S_0[cat_cols].nunique()

# One-hot encoding for categorical variables
cat_0_encoded, cat_1_encoded = {}, {}
for col in cat_cols:
    cat_0_encoded[col] = pd.get_dummies(cat_0[col], prefix=col)
    cat_1_encoded[col] = pd.get_dummies(cat_1[col], prefix=col)

    columns_0 = cat_0_encoded[col].columns
    cat_0_encoded_np_col = sk.preprocessing.scale(cat_0_encoded[col])
    cat_0_encoded[col] = pd.DataFrame(cat_0_encoded_np_col, columns=columns_0)

    columns_1 = cat_1_encoded[col].columns
    cat_1_encoded_np_col = sk.preprocessing.scale(cat_1_encoded[col])
    cat_1_encoded[col] = pd.DataFrame(cat_1_encoded_np_col, columns=columns_1)
    
    # Align categories for the current column
    cat_0_encoded[col], cat_1_encoded[col] = cat_0_encoded[col].align(
        cat_1_encoded[col], join='outer', axis=1, fill_value=0
    )

# Euclidean distance for numerical variables
num_dist = cdist(num_0.to_numpy(), num_1.to_numpy(), metric='euclidean')

# Hamming distance for categorical variables
cat_dists = list()
for col in cat_cols:
    dist = cdist(cat_0_encoded[col].to_numpy(), cat_1_encoded[col].to_numpy(), metric='euclidean')
    cat_dists.append(dist)

# Combine distances
combined_cost = num_dist

for i in range(len(cat_dists)):
  combined_cost += cat_dists[i]

# Transport map
# Uniform weights (equal distribution)
w_0 = ot.unif(len(x_S_0)) # Source weights
w_1 = ot.unif(len(x_S_1)) # Target weights

# Compute transport plan
transport_plan = ot.emd(w_0, w_1, combined_cost, numItermax=1e8)


# Counterfactuals for numerical variables
num_transported = n_0 * transport_plan @ num_1.to_numpy()

# Counterfactuals for categorical variables, then label reconstruction
transported_cats = {}

for col in cat_cols:
    cat_probs = transport_plan @ cat_1_encoded[col].to_numpy()
    cat_columns = cat_1_encoded[col].columns
    transported_cats[col] = pd.Series(cat_probs.argmax(axis=1)).map(lambda x: cat_columns[x].split('_', 1)[1])

# Merge
transformed_data = pd.DataFrame(num_transported, columns=num_cols)

for col in cat_cols:
    transformed_data[col] = transported_cats[col].values

# Export results
csv_file_path = '../data/counterfactuals-ot-nonwhite-compas.csv'
transformed_data.to_csv(csv_file_path, index=False)
