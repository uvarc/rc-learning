---
title: Coding a Neural Network
date: "2022-06-09T00:00:00"
type: docs 
weight: 400
menu: 
    python-machine-learning:
        parent: Neural Networks
---



__Example: Breast Cancer Data__

* The Cancer data set originally was captured at UCI Repository ([https://archive.ics.uci.edu/ml/datasets.html](https://archive.ics.uci.edu/ml/datasets.html))
* Look at the data, so that you understand what is in each file.

{{< table >}} 
| Filename | Brief Description |
| :-: | :-: |
| cancer_data.csv | The table of measurements<br />cancer_DESCR.csv – an overview of the data |
| cancer_feature_names.csv | The names of the columns in cancer_data.csv |
| cancer_target.csv<br /> | The classification (0 or 1) of each row in cancer_data.csv |
| cancer_target_names.csv | The names of the classifications (malignant or benign)<br /> |
{{< /table >}}

## Coding a Neural Network:  General Steps
1. Load the neural network packages
2. Read in the data
3. Divide the data into a training set and a test set.
4. Preprocess the data
5. Design the Network Model
6. Train the model
7. Apply the model to the test data
8. Display the results

### 1. Load Neural Networks Package
```python
from tensorflow import keras
from tensorflow.keras import layers
```
### 2. Read in the Data
```python
import numpy as np
data_file = 'Data/cancer_data.csv'
target_file = 'Data/cancer_target.csv'
cancer_data=np.loadtxt(data_file,dtype=float,delimiter=',')
cancer_target=np.loadtxt(target_file, dtype=float, delimiter=',')
```
### 3. Divide Data
```python
from sklearn import model_selection

test_size = 0.30

seed = 7

train_data, test_data, train_target, test_target = model_selection.train_test_split(cancer_data, cancer_target, test_size=test_size, random_state=seed)
```
### 4. Preprocess the Data
```python
# Convert the classes to 'one-hot' vector
from keras.utils import to_categorical
y_train = to_categorical(train_target, num_classes=2)
y_test = to_categorical(test_target, num_classes=2)
```
### 5. Design the Network Model
```python
def define_model():
    model = keras.Sequential([ 
       layers.Dense(30, activation="relu"), 
       layers.Dense(2, activation="softmax") 
    ])
    model.compile(optimizer="rmsprop", 
              loss="binary_crossentropy",
              metrics=["accuracy"]
    )
    return(model)
    
model = define_model()
```
### 6. Train the Model
```python
num_epochs = 10
batch_size = 32
model.fit(train_data, y_train, epochs=num_epochs, batch_size=batch_size)
```
## 7. Apply the Model to the Test Data
```python
predictions = np.argmax(model.predict(test_data), axis=-1)
```
## 8. Display the Results
```python
score = model.evaluate(test_data, y_test)
print('\nAccuracy:  %.3f' % score[1])
from sklearn.metrics import confusion_matrix
print(confusion_matrix(test_target, predictions))
```
## Activity:  Neural Network Program

Make sure that you can run the Neural Network codes: `03_Neural_Network.ipynb`

