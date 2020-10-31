---
title: "Benchmarking Parallel Programs"
type: article 
toc: true
date: 2020-10-29T00:00:00-05:00

---

# Motivation

# Terms and Implications

Denote the number of cores (or nodes or GPU devices) as N and the walltime as t. The basis of comparison is the serial job where N=1 with a walltime of t1.

**Speedup** is defined as s=t1/t. For example, a job that finishes in half the time has a speedup factor of 2.

**Perfect scaling** is achieved when N=s. If you manage to halve the time (s=2) by doubling the resources (N=2), you achieve perfect scaling.

On Rivanna, the **SU** (service unit) charge rate is defined as
```text
SU = (#core + 2#gpu) x walltime
```

We can define a **relative SU**, i.e. the SU of a parallel job relative to that of its serial reference.
```text
SU     N x t      N
--- = -------- = ---
SU1    1 x t1     s
```
In the case of perfect scaling, N=s and so the relative SU is 1, which means you spend the same amount of SUs for the parallel job as for its serial reference. Since superlinear scaling (s>N) almost never occurs, the implication is that you need to pay an extra price for parallelization. For example, if you double the amount of cores (N=2) but only reduced the walltime by one-third (s=1.5), then the relative SU is equal to N/s=1.33, which means you spend 33% more SUs than a serial job. Whether this is worth it will of course depend on the actual value of s (and your deadline).

# Examples

## PyTorch DistributedDataParallel (DDP)

PyTorch can make use of multiple GPU devices through DDP. This example is based on [our PyTorch 1.7 container](https://hub.docker.com/r/uvarc/pytorch) using the Python script provided by [PyTorch Lightning](https://pypi.org/project/pytorch-lightning) with minor modifications.

### Setup

- Download the container. The following command will create a Singularity container `pytorch_1.7.0.sif`.
```
module load singularity/3.6.1
singularity pull docker://uvarc/pytorch:1.7.0
```

- Copy the following as a plain text file (`example.py`).

```python
import os
import torch
from torch import nn
import torch.nn.functional as F
from torchvision.datasets import MNIST
from torch.utils.data import DataLoader, random_split
from torchvision import transforms
import pytorch_lightning as pl

class LitAutoEncoder(pl.LightningModule):

    def __init__(self):
        super().__init__()
        self.encoder = nn.Sequential(nn.Linear(28 * 28, 128), nn.ReLU(), nn.Linear(128, 3))
        self.decoder = nn.Sequential(nn.Linear(3, 128), nn.ReLU(), nn.Linear(128, 28 * 28))

    def forward(self, x):
        # in lightning, forward defines the prediction/inference actions
        embedding = self.encoder(x)
        return embedding

    def training_step(self, batch, batch_idx):
        # training_step defined the train loop. It is independent of forward
        x, y = batch
        x = x.view(x.size(0), -1)
        z = self.encoder(x)
        x_hat = self.decoder(z)
        loss = F.mse_loss(x_hat, x)
        self.log('train_loss', loss)
        return loss

    def validation_step(self, batch, batch_idx):
        x, y = batch
        x = x.view(x.size(0), -1)
        z = self.encoder(x)
        x_hat = self.decoder(z)
        loss = F.mse_loss(x_hat, x)
        self.log('val_loss', loss)
        return loss

    def configure_optimizers(self):
        optimizer = torch.optim.Adam(self.parameters(), lr=1e-3)
        return optimizer

dataset = MNIST(os.getcwd(), download=True, transform=transforms.ToTensor())
train, val = random_split(dataset, [55000, 5000])

autoencoder = LitAutoEncoder()
trainer = pl.Trainer(max_epochs=1, gpus=1)
trainer.fit(autoencoder, DataLoader(train), DataLoader(val))
```

- Prepare a SLURM script (`job.slurm`) that runs the above Python script. We would like to download the MNIST data files first and exclude the download time from the actual benchmark. (More on this later.) Also fix a GPU type for consistency.

```
#!/bin/bash
#SBATCH -A <myallocation>
#SBATCH -t 00:10:00
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH -p gpu
#SBATCH --gres=gpu:k80:1

module purge
module load singularity/3.6.1

time singularity run --nv pytorch_1.7.0.sif example.py
```

- Submit the job.
```
sbatch job.slurm
```

### Benchmark
Set `download=False` in `example.py`:
```
dataset = MNIST(os.getcwd(), download=False, transform=transforms.ToTensor())
```

Resubmit the job to set the reference (N=1, t1). Next, to make use of multiple GPU devices, use the `ddp` backend:
```
trainer = pl.Trainer(max_epochs=1, gpus=2, distributed_backend='ddp')
```
Also add `srun` to your SLURM script:
```
time srun singularity run --nv pytorch_1.7.0.sif example.py
```

For K80 you should receive similar results as follows:

|N|Time (s)|Speedup|Relative SU|
|---|---|---|---|
|1|226 |1.00   |1.00|
|2|134 |1.69   |1.18|
|3| 95 |2.38   |1.26|
|4| 76 |2.97   |1.35|

The speedup is plotted below. Notice how the deviation from perfect scaling (light diagonal line) increases with N.

{{< figure src="ddp_k80.png" width="400px" >}}

The same benchmark was performed on RTX 2080Ti (coming soon to Rivanna!)

|N|Time (s)|Speedup|Relative SU|
|---|---|---|---|
|1|171|1.00|1.00|
|2|108|1.58|1.26|
|3|79|2.16|1.39|
|4|64|2.67|1.50|
|5|57|3.00|1.67|
|6|52|3.29|1.82|
|7|51|3.35|2.09|
|8|49|3.49|2.29|
|9|49|3.49|2.58|
|10|49|3.49|2.87|

Notice the plateau beyond N=6 - this implies that you should not request more than 6 GPU devices for this particular task.

{{< figure src="ddp_rtx.png" width="400px" >}}

The performance of K80 vs RTX 2080Ti is compared below. On a single GPU device, the latter is 30% faster. 

{{< figure src="k80_rtx.png" width="400px" >}}
