---
date : "2025-02-26T00:00:00"
title: "Transfer Learning"
toc: true
type: book
weight: 20

---

### **What is Transfer Learning?**
Transfer learning allows us to **reuse a pre-trained model** (trained on a large dataset) for a **new but related task** instead of training from scratch.Typically we use foundation models for transfer learning. A foundation model is a large-scale, general-purpose AI model trained on large corpora. It serves as a base model that can be fine-tuned for specific tasks, reducing the need for extensive labeled data. Examples include GPT-4 (text generation), BERT (NLP tasks), and ResNet (computer vision). 

#### **Why Use Transfer Learning?**
 1. **Reduces Training Time:** Pre-trained weights are a better starting off point than random initialization. Since the model has already learned useful features, training takes significantly less time.
 
 2. **Powerful Even with Small Datasets:** Even with limited labeled data, transfer learning improves generalization.  
 3. **Superior Performance**: Many pre-trained models outperform custom-trained models on similar tasks.  
 4. **Reduced Development Time**: Leverages the architecture of models proven to work well on similar data.
 

Common scenarios:
 - Using a **pre-trained image classifier** (e.g., ResNet, VGG) for a new image classification task.
 - Applying a **pre-trained language model** (e.g., BERT, GPT) for sentiment analysis or named entity recognition.

---
### **Loading and Preprocessing Datasets**
Data are available online from places like PyTorch (torchvision, torchtext, timm) , Huggingface, Paperswithcode, etc. For this course, we'll work with torchvision.

#### **Image Datasets with `torchvision`**
PyTorch provides **`torchvision`**, which includes popular datasets and transformations for preprocessing images.

##### **Loading a Dataset (Example: CIFAR-10)**
```python
import torch
import torchvision
import torchvision.transforms as transforms

# Define transformations (resize, normalize)
transform = transforms.Compose([
    transforms.Resize((224, 224)),  # Resize images for pre-trained models
    transforms.ToTensor(),
    transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225])
])

# Load the training dataset
train_dataset = torchvision.datasets.CIFAR10(root="./data", train=True, transform=transform, download=True)
train_loader = torch.utils.data.DataLoader(train_dataset, batch_size=32, shuffle=True)
```


---
### Leveraging Pre-Trained Models for New Tasks
Instead of training from scratch, we can use foundation models from `torchvision.models`.

```python
from torchvision import models

# Load pre-trained ResNet model
resnet = models.resnet50(pretrained=True)

# Print the model architecture
print(resnet)
```

The last layer (fully connected) is task-specific and needs to be modified for a new dataset.
#### Modifying the Last Layer
Since ResNet is trained on ImageNet (1000 classes), we modify the final layer for our dataset.
```python
import torch.nn as nn

num_classes = 10  # For CIFAR-10 classification
resnet.fc = nn.Linear(resnet.fc.in_features, num_classes)
```


#### Fine-Tuning Models Efficiently with PyTorch
Fine-tuning involves unfreezing some layers of a pre-trained model and training it on a new dataset.

##### **Freezing Some Layers**
To avoid losing learned features, we freeze most layers and train only the last few.

```python
for param in resnet.parameters():
    param.requires_grad = False  # Freeze all layers

# Unfreeze only the last layer
for param in resnet.fc.parameters():
    param.requires_grad = True

# Define loss function and optimizer
criterion = nn.CrossEntropyLoss()  # Loss function for classification
optimizer = optim.Adam(resnet.fc.parameters(), lr=0.001)  # Train only last layer

# Training the Model
epochs = 5
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
resnet.to(device)

for epoch in range(epochs):
    for images, labels in train_loader:
        images, labels = images.to(device), labels.to(device)

        optimizer.zero_grad()
        outputs = resnet(images)
        loss = criterion(outputs, labels)
        loss.backward()
        optimizer.step()

    print(f"Epoch {epoch+1}/{num_epochs}, Loss: {loss.item():.4f}")

# Testing
correct = 0
total = 0

resnet.eval()  # Set to evaluation mode
with torch.no_grad():
    for images, labels in train_loader:
        images, labels = images.to(device), labels.to(device)
        outputs = resnet(images)
        _, predicted = torch.max(outputs, 1)  # Get highest probability class
        total += labels.size(0)
        correct += (predicted == labels).sum().item()

print(f"Accuracy: {100 * correct / total:.2f}%")
```
NOTE: For best model performance, it is important that your data is in the same format as the data used to train the model and that you perform the same transformations on your training data as the original model. For example, ResNet was originally trained on the ImageNet dataset which makes the standard input of Resnet a 224x224 pixel image in RGB.
