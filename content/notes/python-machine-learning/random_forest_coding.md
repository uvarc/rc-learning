---
title: Coding a Random Forest
date: "2022-06-09T00:00:00"
type: docs 
weight: 300
menu: 
    python-machine-learning:
      parent: Random Forest
---


## The Data

For the Random Forest example, we will reuse the `winequality_red` data set.

## Coding Random Forest:  General Steps
1. Load the random forest packages
2. Read in the data
3. Identify the target feature
4. Divide the data into a training set and a test set.
  a. Choose the sample size
  b. Randomly select rows
  c. Separate the data
5. Fit the random forest model
6. Apply the model to the test data
7. Display the feature importance

### 1. Load Random Forest Package
```python
from sklearn.ensemble import RandomForestClassifier
```

### 2, 3, 4.
```python
# Repeat steps 2-4 from the Decision Tree example
import pandas as pd
. . .
train_data, test_data, train_target, test_target = model_selection.train_test_split(wine_data, wine_target, test_size=test_size, random_state=seed)
```

### 5. Fit the Random Forest Model
```python
model = RandomForestClassifier()
model.fit(train_data, train_target)
```

### 6. Apply the Model to the Test Data
```python
forest_results = model.predict(test_data)
```

### 7. Compute Feature Importance
```python
importances = model.feature_importances_
```

### 8. List Feature Importance
```python
import numpy as np
indices = np.argsort(importances)[::-1]
print("Feature ranking:")
col_names = list(train_data.columns.values)
for f in range(len(indices)):
feature = col_names[indices[f]]
space = ' '*(20 - len(feature))
print("%d.\t %s %s (%f)" % \
(f + 1, feature, space, importances[indices[f]]))
```

## Activity:  Random Forest Program

Make sure that you can run the Random Forest code: `02_Random_Forest.ipynb`


