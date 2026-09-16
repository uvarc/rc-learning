---
title: Numba CUDA Example
date: 2026-09-2T17:26:29Z
type: book 
weight: 6550
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

A Numba CUDA example for fast matrix multiplication is part of the documentation distributed with the package.

{{< code-download file="/courses/parallel-computing-introduction/codes/cuda_numba_example.py" lang="python" >}}

**Discussion**

This example sets the threads per block to 16 and we divide the computation into TPBxTPB chunks.  

We import Numba CUDA with the `cuda.jit` decorator.  We then establish the grid and assign thread blocks to it.  Each thread block will compute an element.

The shared array is allocated on the device in a memory level that can be accessed by all threads in a block.  This is faster than transferring data or allocating on the global device memory. 

In the main program, we create three arrays on the host and then use `to_device` to copy them to the device global memory.  The device arrays are passed to the `fast_matmul` function, which performs the computation. We then copy only the result back to the host with `copy_to_host`.

The `@` operator was introduced in NumPy 1.10.0 for two-d matrix multiplication. For those not familiar with it, it is "syntactic sugar" for `numpy.matmul`.

Full documentation for Numba CUDA is available from [NVIDIA](https://nvidia.github.io/numba-cuda/index.html).
