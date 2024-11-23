---
title: Coding a Tensor Flow
date: "2022-06-09T00:00:00"
type: docs 
weight: 500
menu: 
    python-machine-learning:
      parent: TensorFlow
---



## General Steps
1. Load the neural network packages
2. Read in the data
3. Divide the data into a training set and a test set.
4. Preprocess the data
5. Design the Network Model
6. Train the model
7. Apply the model to the test data
8. Display the results

### 1. Load Keras Packages
```python
from tensorflow import keras
from tensorflow.keras import layers
from tensorflow.keras.utils import to_categorical
```

### 2. Read in the Data
```python
import numpy as np
data_file = 'Data/cancer_data.csv'
target_file = 'Data/cancer_target.csv'
cancer_data=np.loadtxt(data_file,dtype=float, delimiter=',')
cancer_target=np.loadtxt(target_file, dtype=float, delimiter=',')
```

### 3. Split the Data
```python
from sklearn import model_selection
test_size = 0.30
seed = 7
train_data, test_data, train_target, test_target = model_selection.train_test_split(cancer_data,cancer_target, test_size=test_size, random_state=seed)
```

### 4. Pre-process the Data
```python
from sklearn.preprocessing import StandardScaler
scaler = StandardScaler()
# Fit only to the training data
scaler.fit(train_data)
# Now apply the transformations to the data:
x_train = scaler.transform(train_data)
x_test = scaler.transform(test_data)
# Convert the classes to 'one-hot' vector
y_train = to_categorical(train_target, num_classes=2)
y_test = to_categorical(test_target, num_classes=2)
```

### 5. Define the Model
```python
def define_model():
  from tensorflow.keras.models import Sequential
  from tensorflow.keras.layers import Dense, Dropout
  from tensorflow.keras.optimizers import SGD

  model = keras.Sequential([ 
    layers.Dense(30, activation="relu"),
    layers.Dropout(0.5),
    layers.Dense(60, activation="relu"),
    layers.Dropout(0.5),
    layers.Dense(2, activation="softmax")
  ])

  model.compile(optimizer="rmsprop", loss="binary_crossentropy", metrics=["accuracy"])

  return(model)

model = define_model()
```

### 7.Fit the Model
```python
b_size = int(.8*x_train.shape[0])
num_epochs = 20
model.fit(x_train, y_train, epochs=num_epochs, batch_size=b_size)
```

### 8.Apply the Model to Test Data
```python
predictions = np.argmax(model.predict(x_test), axis=-1)
```

### 9.Evaluate the Results
```python
score = model.evaluate(x_test, y_test, batch_size=b_size)
print('\nAccuracy:  %.3f' % score[1])
from sklearn.metrics import confusion_matrix
print(confusion_matrix(test_target, predictions))
```

## Activity:  TensorFlow Program

Make sure that you can run the TensorFlow code: `04_TensorFlow.ipynb`

