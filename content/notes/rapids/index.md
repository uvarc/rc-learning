---
title: "RAPIDS"
type: article 
toc: true
date: 2021-11-07T00:00:00-05:00

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

We have also prepared a notebook `cudf.ipynb` under `/project/apps_data/rapids`. 

1. In `cudf.ipynb`, compare the performance of `pandas` and `cudf` starting from `N = 100`. Explore the behavior by varying `N`. Beyond which order of magnitude does `cudf` outperform `pandas`?

1. Which method's relative performance remains fairly constant? In other words, the ratio of the `pandas` execution time to the `cudf` execution time does not change much with respect to `N`.

1. Which method has the highest performance boost using `cudf`?

1. Repeat all of the above for other data types.

1. In `cuml/notebook/kmeans_demo.ipynb`, compare the performance of `scikit-learn` and `cuml` by varying `N`. Beyond which order of magnitude does `cuml` outperform `scikit-learn`?

1. There is a cell that checks the accuracy of `cuml` versus `scikit-learn`. Is this necessary?

1. Feel free to explore other notebooks.

1. General question: Why does the performance of RAPIDS depend on `N`? Why does the CPU API outperform RAPIDS when `N` is not big enough?

# Remark: JupyterLab vs batch job

The JupyterLab environment is interactive which is great for debugging and testing. However, if the queue is busy you may need to wait for a long time. If your code can be executed non-interactively, we recommend converting it into a Python script so that you can submit it as a batch job. 

## Converting a notebook into a Python script

The following command will convert your notebook `mynotebook.ipynb` into `mynotebook.py`.

```bash
module load anaconda
jupyter nbconvert --to script mynotebook.ipynb
```

You may need to comment out Jupyter magic commands (e.g. `%%time`) before converting.

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

---

# References and further reading

- [Official documentation](https://docs.rapids.ai/)
- [Workshop: High Performance Programming in Python](/workshops/python-hi-perf/)
