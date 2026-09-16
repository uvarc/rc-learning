---
title: CUDA with Python
date: 2026-09-2T17:26:29Z
type: book 
weight: 6500
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

Currently the best-supported method of programming NVIDIA GPUs is the NVIDIA [CUDA Python](https://developer.nvidia.com/cuda/python) initiative.

The package can be installed with `pip`

```bash
pip install cuda-python
```

This installs a number of CUDA-related libraries and APIs. This package is intended to provide an NVIDIA-supported layer for Python bindings to CUDA that do not need to go through C or C++.

For a more complete installation that includes the CUDA Core Compute Libraries and the CUDA backend for numba, use

```bash
pip install cuda-python cuda-cccl numba-cuda
```

A specific version of CUDA can be specified with `numba-cuda[cu12]` or `numba-cuda[cu13]`.

Beyond CUDA 13 NVIDIA is moving to MLIR (Multi-Level IR Compiler Framework) so this will change to

```bash
pip install cuda-python cuda-cccl numba-cuda-mlir[cu13]
```

The CCCL is a package of optimized parallel libraries for basic computing functionality such as sorting, scanning, graphs, etc.

## Numba

[Numba](https://numba.pydata.org/) is a JIT (just-in-time) compiled version of core NumPy routines. It is widely used to optimize critical parts of numerically-oriented Python codes for high performance. With the CUDA backend it can compile for NVIDIA GPUs.

It is invoked with a decorator
```python
#On CPU
from numba import *
@njit
def func():

#On GPU
@cuda.jit
def gpu_func():
```

## CuPy

[CuPy](https://cupy.dev/) is a package that implements a subset of NumPy and SciPy array functions on a GPU.  It provides drop-in replacement functions.

```python
import numpy as np
import cupy as cp
A=cp.arange(12).reshare(4,3)
```

The CuPy arrays are allocated on the _current device_, not on the host. If a system has only one device, that will be device 0. If multiple devices are present the one reported as default by the system will be 0, the next 1, and so forth.

NumPy is required.  SciPy is optional. Another optional package is [Optuna](https://optuna.org/), a package for hyperparameter optimization for deep learning.

CuPy can also be installed with `pip`. It must be installed for a specific CUDA version, here 13:
```bash
pip install cupy-cuda13x
```
It is also available for AMD's ROCm
```bash
pip install cupy-rocm-7-0
```

A CUDA toolkit (or equivalent for ROCm) must be installed for CuPy to function.

CuPy can use optional libraries, for example [cuSPARSELt](https://docs.nvidia.com/cuda/cusparselt/) for sparse-matrix multiplication. Another library might be useful for higher-end NVIDIA GPUs is [cuTENSOR](https://developer.nvidia.com/cutensor) while another that would be mainly used on HPC systems with multiple NVIDIA cards per node and/or multiple nodes with GPUs is [NCCL](https://developer.nvidia.com/nccl). 

To install one or more of these libraries into your Python environment, use
```bash
pip install "cutensor-cu13"
pip install "nvidia-cusparselt-cu13"
pip install "nvidia-nccl-cu13"
```
with appropriate changes for CUDA version.
.
