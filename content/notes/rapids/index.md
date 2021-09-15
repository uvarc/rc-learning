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
| [cuSpatial](https://github.com/rapidsai/cuspatial) | | |
| [cuxfilter](https://github.com/rapidsai/cuxfilter) | crossfilter | |

We will focus on the first three components in this introductory workshop. The others are more specialized.

## Prerequisites

- NVIDIA GPU with compute capability 6.0+ (Pascal)
    - On Rivanna, RAPIDS will not work on K80 nodes. If you're using JupyterLab, exclude them by specifying
        ```
        -x udc-ba25-2[3,7,8],udc-ba26-2[3-6],udc-ba27-2[3-4]
        ```
        in the "Optional: Slurm Option" box.

        If you're submitting a batch job, add this line to your SLURM script:
        ```
        #SBATCH -C "p100|v100|rtx2080"
        ```
- CUDA 11

## Installation

The `rapidsai` module is available on Rivanna. It is backed by a [container](https://github.com/uvarc/rivanna-docker/tree/master/rapidsai) that we built. (The container image is 40% smaller than the official one. [Learn more about containers](/workshops/building-containers) to understand why.)

To install on your own machine please visit <https://rapids.ai/start.html>.


