---
title: "Benchmarking Parallel Programs"
type: article 
toc: true
date: 2020-10-29T00:00:00-05:00

---

# Motivation

True or false:

1. "If I use more cores/GPUs, my job will run faster."
1. "I can save SUs by using more cores/GPUs, since my job will run faster."
1. "I should always use all cores/GPUs on a node."

The answer will of course depend on your program, but many users implicitly assume that these statements are true. (1) is **not** guaranteed. (2) is false. (3) depends.

The premise of parallel scalability is that the program has to be _parallelizable_. Please read the documentation of your program to determine whether there is any benefit of using multiple cores/GPUs. (Note: A serial program may need more memory than a single core can provide. You will need to request multiple cores, but this is a memory-bound issue and is different from the execution speed.)

# Concepts

## Definitions
Denote the number of cores (or nodes or GPU devices) as $N$ and the walltime as $t$. The basis of comparison is the serial job where $N=1$ with a walltime of $t_1$.

**Speedup** is defined as $s=t_1/t$. For example, a job that finishes in half the time has a speedup factor of 2.

**Perfect scaling** is achieved when $N=s$. If you manage to halve the time ($s=2$) by doubling the resources ($N=2$), you achieve perfect scaling.

On Rivanna, the **SU** (service unit) charge rate is defined as
$$SU = (N_{\mathrm{core}} + 2N_{\mathrm{gpu}}) t$$

We can define a **relative SU**, i.e. the SU of a parallel job relative to that of its serial reference.
$$\frac{SU}{SU_1} = \frac{Nt}{t_1} = \frac{N}{s}$$

In the case of perfect scaling, $N=s$ and so the relative SU is 1, which means you spend the same amount of SUs for the parallel job as for its serial reference. Since sublinear scaling ($s<N$) almost always occurs, the implication is that you need to pay an extra price for parallelization. For example, if you double the amount of cores ($N=2$) and reduce the walltime by only one-third ($s=1.5$), then the relative SU is equal to $N/s=1.33$, which means you spend 33% more SUs than a serial job. Whether this is worth it will of course depend on (1) the actual value of $s$, (2) the maximum walltime limit for the partition on Rivanna, and (3) your deadline.

## Amdahl's Law (strong scaling)

A portion of a program is called parallel if it can be parallelized. Otherwise it is said to be serial. In this simple model, a program is strictly divided into parallel and serial portions. Denote the parallel portion as $p$ ($0 \le p \le 1$) and the serial portion as $1-p$ (so that the sum equals 1).

Suppose the program takes a total execution time of $t_1$ to run completely in serial. Then the execution time of the parallelized program can be expressed as a function of $N$:
$$t = \left[(1-p) + \frac{p}{N}\right] t_1$$

The speedup is thus
$$s=\frac{t_1}{t} = \frac{1}{1-p+\frac{p}{N}}$$

As $N\rightarrow\infty$, $s\rightarrow 1/(1-p)$. This is the theoretical speedup limit.

**Exercise:** Find the maximum speedup if 99%, 90%, 50%, 10%, and 0% of the program is parallelizable.

# Tools

The performance is usually tracked by the execution time.

## `time`

The `time` command is included in most Linux distributions. You can simply prepend it to whatever command you want to time. For example:

```bash
$ time sleep 5

real    0m5.026s
user    0m0.001s
sys     0m0.006s
```

Notice there are 3 lines of output - real, user, and sys. A good explanation of these can be found [here](https://stackoverflow.com/questions/556405/what-do-real-user-and-sys-mean-in-the-output-of-time1); for this tutorial, it is sufficient to use the real time. (The CPU time is given by user + sys, which in this case is almost 0 because the command is just `sleep`.)

## `perf`

A more dedicated tool for performance measurement is `perf` (not on Rivanna). Advanced users please refer to the official [tutorial](https://perf.wiki.kernel.org/index.php/Tutorial).

# Examples

## Multi-GPU: PyTorch

PyTorch can make use of multiple GPU devices through the DistributedDataParallel (DDP) backend. This example is based on [our PyTorch 1.7 container](https://hub.docker.com/r/uvarc/pytorch) using the Python script provided by [PyTorch Lightning](https://pypi.org/project/pytorch-lightning) with minor modifications. (The container uses CUDA 11 which is compatible with Rivanna hardware after the December 2020 maintenance.)

### Setup

- Download the container. The following command will create a Singularity container `pytorch_1.7.0.sif`.
```bash
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

- Prepare a SLURM script (`job.slurm`) that runs the above Python script. We would like to download the MNIST data files first and exclude the download time from the actual benchmark. (More on this later.) Also specify a GPU type for consistency.

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
```bash
sbatch job.slurm
```

### Benchmark
Set `download=False` in `example.py`:
```python
dataset = MNIST(os.getcwd(), download=False, transform=transforms.ToTensor())
```

Resubmit the job to set the reference (N=1, t1). Next, to make use of multiple GPU devices, use the `ddp` backend:
```python
trainer = pl.Trainer(max_epochs=1, gpus=2, distributed_backend='ddp')
```
For this example, use the same number in `--ntasks-per-node` and `--gres=gpu` in your SLURM script. Also add `srun` (**important**):
```bash
time srun singularity run --nv pytorch_1.7.0.sif example.py
```

For K80 you should obtain similar results as follows:

|N|Time (s)|Speedup|Relative SU|
|---|---|---|---|
|1|226 |1.00   |1.00|
|2|134 |1.69   |1.18|
|3| 95 |2.38   |1.26|
|4| 76 |2.97   |1.35|

The speedup is plotted below. Notice how the deviation from perfect scaling (light diagonal line) increases with $N$.

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

Notice the plateau beyond $N=6$ - this implies that you should not request more than 6 GPU devices for this particular task. (A good balance between speed and SU effectiveness may be $2\le N \le 4$.)

{{< figure src="ddp_rtx.png" width="400px" >}}

**Exercise:** Deduce the parallel portion $p$ of this program using Amdahl's Law.

The performance of K80 vs RTX 2080Ti is compared below. On a single GPU device, the latter is 30% faster. 

{{< figure src="k80_rtx.png" width="400px" >}}

### Remarks

A complete machine learning benchmark would involve such parameters as batch size, learning rate, etc. You may pass a sparse grid to locate a desirable region and, if necessary, use a finer grid in that region to identify the best choice.
