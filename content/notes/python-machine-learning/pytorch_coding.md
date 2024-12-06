---
title: Coding a PyTorch
date: "2022-06-09T00:00:00"
type: docs 
weight: 600
menu: 
    python-machine-learning:
      parent: PyTorch
---


## General Steps
1. Import the torch package
2. Read in the data
3. Preprocess the data
&nbsp; 3a. Scale the data
&nbsp; 3b. Split the data
&nbsp; 3c. Convert data to tensors
&nbsp; 3d. Load the tensors
4. Design the Network Model
5. Define the Learning Process
6. Train the model
7. Apply the model to the test data
8. Display the results

### 1. Import torch Package
```python
import torch
if torch.cuda.is_available():
  device_type = "cuda:" + str(torch.cuda.current_device())
else:
  device_type = "cpu"

device = torch.device(device_type)
```

### 2. Read in the Data
```python
import numpy as np
data_file = 'Data/cancer_data.csv'
target_file = 'Data/cancer_target.csv'
x=np.loadtxt(data_file,dtype=float,delimiter=',')
y=np.loadtxt(target_file, dtype=float, delimiter=',')
print("shape of x: {}\nshape of y: {}".format(x.shape,y.shape))
```

### 3a. Scale the data
```python
#feature scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
x = sc.fit_transform(x)
```

### 3b. Split the Data
```python
from sklearn import model_selection
test_size = 0.30
seed = 7
train_data, test_data, train_target, test_target = model_selection.train_test_split(x, y, test_size=test_size, random_state=seed)
```

### 3c. Convert data to tensors
```python
#defining dataset class
from torch.utils.data import Dataset

class dataset(Dataset):
  def __init__(self,x,y):
    self.x = torch.tensor(x,dtype=torch.float32)
    self.y = torch.tensor(y,dtype=torch.float32)
    self.length = self.x.shape[0]

  def __getitem__(self,idx):
    return self.x[idx],self.y[idx]

  def __len__(self):
    return self.length

trainset = dataset(train_data,train_target)
```

### 3d. Load the tensors
```python
#DataLoader
from torch.utils.data import DataLoader

trainloader = DataLoader(trainset,batch_size=64,shuffle=False)
```

### 4. Design the Network Model
```python
from torch import nn

class Net(nn.Module):
  def __init__(self,input_shape):
    super(Net,self).__init__()
    self.fc1 = nn.Linear(input_shape,32)
    self.fc2 = nn.Linear(32,64)
    self.fc3 = nn.Linear(64,1)

  def forward(self,x):
    x = torch.relu(self.fc1(x))
    x = torch.relu(self.fc2(x))
    x = torch.sigmoid(self.fc3(x))
    return x

model = Net(input_shape=x.shape[1])
```

### 5. Define the Learning Process
```python
learning_rate = 0.01
epochs = 700

optimizer = torch.optim.SGD(model.parameters(), lr=learning_rate)
loss_fn = nn.BCELoss()
```

### 6. Fit the Model
```python
losses = []
accur = []

for i in range(epochs):
  for j,(x_train,y_train) in enumerate(trainloader):
    
    #calculate output
    output = model(x_train)

    #calculate loss
    loss = loss_fn(output,y_train.reshape(-1,1))

    #accuracy
    predicted = model(torch.tensor(x,dtype=torch.float32))
    acc = (predicted.reshape(-1).detach().numpy().round() == y).mean()
    #backprop
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

if i%50 == 0:
  losses.append(loss)
  accur.append(acc)
  print("epoch {}\tloss : {}\t accuracy : {}".format(i,loss,acc))
```

### 7. Apply the Model to Test Data
```python
testset = dataset(test_data,test_target)
trainloader = DataLoader(testset,batch_size=64,shuffle=False)
predicted = model(torch.tensor(test_data,dtype=torch.float32))
```

### 8. Evaluate the Results
```python
acc = (predicted.reshape(-1).detach().numpy().round() == test_target).mean()

print('\nAccuracy:  %.3f' % acc)

from sklearn.metrics import confusion_matrix
predicted = predicted.reshape(-1).detach().numpy().round()

print(confusion_matrix(test_target, predicted))
```


## Activity:  PyTorch Program

Make sure that you can run the PyTorch code: `06_PyTorch.ipynb`


