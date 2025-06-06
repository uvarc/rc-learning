---
date : "2025-02-26T00:00:00"
title: "PyTorch Experimentation and Project Setup"
toc: true
type: book
weight: 30

---


### **Structuring a PyTorch Project**
A well-organized project structure makes debugging, collaboration, and experimentation easier.
```
pytorch_project/
├── data/                 # Dataset files
├── logs/                 # TensorBoard/WandB logs
├── models/               # Trained models
├── notebooks/            # Jupyter notebooks
├── src/                  # Source code
│   ├── dataset.py        # Custom dataset loaders
│   ├── model.py          # Neural network architectures
│   ├── train.py          # Training loop
│   ├── evaluate.py       # Model evaluation
│   ├── utils.py          # Utility functions (checkpointing, logging)
├── requirements.txt      # Dependencies
├── config.yaml           # Hyperparameter config
├── train.py              # Main training script
├── slurm_train.sh        # SLURM script for HPC training
└── README.md             # Documentation
```
---
### Managing Experiments with Logging Frameworks
Tracking experiments is essential for understanding model performance.

Using TensorBoard for Monitoring
TensorBoard is Tensorflow's visualization toolkit. It is compatible with PyTorch and enbles you to keep track of your experiments' metrics like lossand accuracy and easily visualize them. For more information visit: https://www.tensorflow.org/tensorboard
```python
from torch.utils.tensorboard import SummaryWriter

writer = SummaryWriter("logs/experiment_1")

# Log loss values
for epoch in range(10):
    loss = 0.1 * epoch  # Example loss
    writer.add_scalar("Loss/train", loss, epoch)

writer.close()
```
To visualize logs, run:
~~~sh
tensorboard --logdir=logs
~~~
Using Weights & Biases for Experiment Tracking. [Learn More](https://docs.wandb.ai/)
```python
import wandb

wandb.init(project="pytorch-experiments")

for epoch in range(10):
    wandb.log({"loss": 0.1 * epoch})
```
---
### Creating Reproducible Experiments
##### Ensure reproducibility by setting random seeds.
```python
import torch
import random
import numpy as np

def set_seed(seed=42):
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    np.random.seed(seed)
    random.seed(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

set_seed(42)
```
##### Ensuring Deterministic Data Pipelines
In PyTorch's DataLoader, the shuffle parameter controls whether the data is randomly shuffled before each epoch. When shuffle is set to True, the data is reshuffled at the beginning of each epoch, ensuring that the model sees the data in a different order during training. Use shuffle for training sets and ensure that it is set to False for test and validation sets. 
```python
train_loader = torch.utils.data.DataLoader(train_dataset, batch_size=32, shuffle=True)
valid_loader = torch.utils.data.DataLoader(valid_dataset, batch_size=32, shuffle=False)
test_loader = torch.utils.data.DataLoader(test_dataset, batch_size=32, shuffle=False)
```

