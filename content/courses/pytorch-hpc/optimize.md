---
date : "2025-02-26T00:00:00"
title: "Optimizing Neural Networks"
toc: true
type: book
weight: 25

---


### **Optimizers and Learning Rate Adjustments**
Optimizers control how a model updates its weights during training. Choosing the right optimizer and learning rate can significantly impact training efficiency.

#### **Types of Optimizers**
| Optimizer  | Description |
|------------|------------|
| **SGD (Stochastic Gradient Descent)** | Standard optimizer, may converge slowly but generalizes well. |
| **Momentum-Based SGD** | Uses past gradients to accelerate learning. |
| **Adam (Adaptive Moment Estimation)** | Adjusts learning rate dynamically, commonly used. |
| **RMSprop** | Handles non-stationary learning rates, useful for RNNs. |


#### Learning Rate Schedulers
Schedulers adjust learning rates during training to improve convergence.
```python
from torch.optim.lr_scheduler import StepLR

scheduler = StepLR(optimizer, step_size=10, gamma=0.1)  # Reduce LR every 10 epochs
```
##### Common Learning Rate Strategies
- Step Decay: Reduces learning rate at regular intervals.
- Exponential Decay: Gradually decreases learning rate.
- Cyclic Learning Rates: Alternates between high and low learning rates.
---
### Other Techniques for Avoiding Overfitting
Overfitting occurs when a model performs well on training data but poorly on new data. Below are effective strategies to prevent overfitting.

#### A. Dropout Regularization
Randomly drops neurons during training, forcing the network to learn more robust features.
```python
import torch.nn as nn

model = nn.Sequential(
    nn.Linear(256, 128),
    nn.ReLU(),
    nn.Dropout(0.5),  # 50% dropout
    nn.Linear(128, 10)
)
```

#### B. Batch Normalization
Normalizes activations across mini-batches, improving training stability.
```python
model = nn.Sequential(
    nn.Linear(256, 128),
    nn.BatchNorm1d(128),  # Normalize layer activations
    nn.ReLU(),
    nn.Linear(128, 10)
)
```

#### C. L2 Regularization (Weight Decay)
Adds a penalty to large weight values, reducing overfitting.
```python
optimizer = optim.Adam(model.parameters(), lr=0.001, weight_decay=1e-5)
```
---
### Importance of Validation Sets
A validation set is crucial for tuning hyperparameters and detecting overfitting by assessing a model's generalization ability on unseen data. Unlike the training set, which optimizes model parameters, the validation set helps select hyperparameters such as learning rate, batch size, and regularization strength using techniques like grid search or Bayesian optimization. It also helps identify overfitting by comparing training and validation performance—if the model performs well on training data but poorly on validation data, overfitting is likely. Strategies such as early stopping, dropout, and regularization can help mitigate this. Additionally, early stopping prevents excessive training once validation performance stops improving. After tuning, a separate test set evaluates the final model’s true performance. A validation set is essential for optimizing hyperparameters, preventing overfitting, and ensuring strong generalization.

A common data split:
 - Training Set (70-80%) – Used to train the model.
 - Validation Set (10-20%) – Used to tune hyperparameters.
 - Test Set (10-20%) – Used for final evaluation.
#### Using a Validation Set in PyTorch
```python
from sklearn.model_selection import train_test_split

train_data, val_data = train_test_split(train_dataset, test_size=0.2, random_state=42)

train_loader = torch.utils.data.DataLoader(train_data, batch_size=32, shuffle=True)
val_loader = torch.utils.data.DataLoader(val_data, batch_size=32, shuffle=False)
```
---
### Data Augmentation Techniques
Data augmentation artificially expands training data by applying transformations. For image data these augmentations exist within transforms. You can compose a transforms pipeline that adds noise to your data, making it more likely to generalize well.

#### Data Augmentation with `torchvision.transforms`
```python
import torchvision.transforms as transforms

transform = transforms.Compose([
    transforms.RandomHorizontalFlip(),  # Flip image horizontally
    transforms.RandomRotation(30),  # Rotate image
    transforms.ColorJitter(brightness=0.2),  # Adjust brightness
    transforms.ToTensor()
])
```
Other data types have their own augmentations.

---
### Handling Imbalanced Data
Class imbalance (e.g., detecting rare diseases) can lead to biased predictions.

#### Using Weighted Loss Functions
Assign higher weights to minority classes.
```python
class_weights = torch.tensor([0.1, 0.9])  # Adjust for class imbalance
criterion = nn.CrossEntropyLoss(weight=class_weights)
```
#### Oversampling Minority Classes
Duplicate underrepresented samples.
```python
from torch.utils.data import WeightedRandomSampler

class_counts = [1000, 200]  # Example: Majority vs. Minority class
weights = 1. / torch.tensor(class_counts, dtype=torch.float)
sampler = WeightedRandomSampler(weights, len(weights))

train_loader = DataLoader(dataset, batch_size=32, sampler=sampler)
```
#### SMOTE (Synthetic Minority Over-sampling Technique)
Creates synthetic samples for the minority class.
```python
from imblearn.over_sampling import SMOTE

smote = SMOTE()
X_resampled, y_resampled = smote.fit_resample(X, y)
```


