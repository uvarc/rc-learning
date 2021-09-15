---
title: "RAPIDS"
type: article 
toc: true
date: 2020-11-07T00:00:00-05:00

---

# What is RAPIDS?

RAPIDS is a suite of open source software libraries developed by NVIDIA to accelerate data science pipeline on GPUs. Each component is modeled after its CPU counterpart with minimal code change for the user.

## Components

|Component| CPU API | Note|
|---|---|---|
| [cupy](https://github.com/cupy/cupy) | NumPy, SciPy | Technically not part of RAPIDS |
| [cuDF](https://github.com/rapidsai/cudf) | Pandas | |
| [cuML](https://github.com/rapidsai/cuml) | Scikit-learn | |
| [cuGraph](https://github.com/rapidsai/cugraph) | NetworkX | |
| [cuSignal](https://github.com/rapidsai/cusignal) | SciPy Signal| |
| [cuSpatial](https://github.com/rapidsai/cuspatial) | GeoPandas | |
| [cuxfilter](https://github.com/rapidsai/cuxfilter) | crossfilter | |

We will focus on the first three components in this introductory workshop. The others are more specialized.

## Prerequisites

- NVIDIA GPU with compute capability 6.0+ (Pascal)
    - On Rivanna, RAPIDS will not work on K80 nodes. If you're using JupyterLab, exclude them by specifying
        ```bash
        -x udc-ba25-2[3,7,8],udc-ba26-2[3-6],udc-ba27-2[3-4]
        ```
        in the "Optional: Slurm Option" box.

        If you're submitting a batch job, add this line to your SLURM script:
        ```bash
        #SBATCH -C "p100|v100|rtx2080"
        ```
- CUDA 11

## Installation

The `rapidsai` module is available on Rivanna. It is backed by a [container](https://github.com/uvarc/rivanna-docker/tree/master/rapidsai) that we built. (The container image is 40% smaller than the official one. [Learn more about containers](/workshops/building-containers) to understand why.)

To install on your own machine please visit <https://rapids.ai/start.html>.

# Usage

## JupyterLab

Select the "RAPIDS x.y" kernel.

## Command line

```bash
module load singularity rapidsai
```

# Exercises

A clone of <https://github.com/rapidsai/notebooks> is located at `/project/apps_data/rapids/notebooks`. The full size is near 1 GB. For this workshop, just copy the `cudf` and `cuml` subdirectories:

```bash
cp -r /project/apps_data/rapids/notebooks/repos/{cudf,cuml} ~
```

We have also prepared a notebook under `/project/apps_data/rapids`. 


# Remark: JupyterLab vs batch job

The JupyterLab environment is interactive which is great for debugging and testing. However, if the queue is busy you may need to wait for a long time. If your code can be executed non-interactively, we recommend converting it into a Python script so that you can submit it as a batch job. 

## Converting a notebook into a Python script

The following command will convert your notebook `mynotebook.ipynb` into `mynotebook.py`.

```bash
module load anaconda
jupyter nbconvert --to script mynotebook.ipynb
```

## Batch job

Prepare a SLURM script `job.slurm`:

```bash
#!/bin/bash
#SBATCH -A mygroup             # your allocation account
#SBATCH -p gpu                 # partition
#SBATCH --gres=gpu:1           # number of GPUs
#SBATCH -C "p100|v100|rtx2080" # do not run on K80 nodes
#SBATCH -N 1                   # number of nodes
#SBATCH -c 1                   # number of cores
#SBATCH -t 10:00:00            # time

module purge
module load singularity rapidsai

# change x.y to the actual version
singularity run --nv $CONTAINERDIR/rapidsai-x.y.sif mynotebook.py
```

Submit the job via `sbatch job.slurm`.
