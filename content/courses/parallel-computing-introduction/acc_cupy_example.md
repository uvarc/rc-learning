---
title: CuPy Example
date: 2026-09-11T17:26:29Z
type: book 
weight: 6650
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

This example can be found, along with several others, at the CuPy [github site](https://github.com/cupy/cupy/tree/main/examples).

{{< code-download file="/courses/parallel-computing-introduction/codes/cuda_cupy.py" lang="python" >}}

**Discussion**

CuPy implements many NumPy built-in functions on the GPU using a very similar syntax. It has a few distinct features such as device specification and data transfer between host and device.

This example generates a random matrix `A` and vector `x`, uses them to compute a vector `b`, then solves the linear system `Ax=b` using the computed `b` as the right-hand side, then compares the computed `b` to the constructed one.  It is mostly like NumPy and the `fit` function can be used with either CPU-based arrays or GPU-based arrays.  The `run` routine uses `np.astype` to generate the ndarrays on the host and `cupy.asarray` to generate equivalents on the GPU; `asarray` allocated device memory. The `get_array_module` function returns whether the array argument is a NumPy or CuPy ndarray, and the new vector is assigned to the same location.
