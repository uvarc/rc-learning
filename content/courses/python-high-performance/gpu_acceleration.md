---
title: "GPU acceleration"
toc: true
type: docs
weight: 60
date: "2020-11-17T00:00:00"
menu:
    hp-python:
        parent: High-Performance Python
---

Certain tasks can be greatly accelerated if run on a graphics processing unit (GPU).  A GPU can be regarded as a device that runs hundreds or thousands of threads.  The memory per thread is usually fairly limited but has a very high bandwidth.  Data must be moved to and from the host computer's memory to the GPU's memory.

GPU programming is an advanced topic and we will mention here only a few basics, and some resources for the interested reader.

In what follows, the _host_ is the computer in which the GPU is installed, and the GPU itself is the _device_.  The GPU has its own memory system and cannot access the RAM of the host; similarly the host cannot directly access GPU memory. Data must be copied back and forth between them.

In most cases, it may be advisable to set up a separate environment for different Python+CUDA packages.

## CuPy

[CuPy](https://cupy.dev/) is an implementation of many of the features of NumPy and SciPy that takes advantage of the GPU.  It is one of the simplest introductions to GPU programming.
It can be [installed](https://docs.cupy.dev/en/stable/install.html) with conda through the `conda-forge` channel.
If using the Anaconda Navigator GUI, install the channel, then switch to it and install cuPy through the interface.  For installing from the command line, use
```bash
conda install -c conda-forge cupy
```
You can also use pip to install CuPy.
Alternatively, use a Docker [container](https://hub.docker.com/r/cupy/cupy/).

You must set the `CUDA_PATH` environment variable for CuPy to be able to accelerate your code properly. If you are working with your own computer, CUDA is installed from NVIDIA packages. 

For example, on a local Linux workstation, NVIDIA installs into `/usr/local/cuda` so you should set this as your CUDA_PATH.
```bash
export CUDA_PATH=/usr/local/cuda
```
Refer to NVIDIA's instructions for other operating systems.

On a system such as UVA's HPC environment, the CUDA module will set the CUDA_PATH environment variable.
```bash
module load cuda
```

Methods invoked through the CuPy module will be carried out on the GPU.  Corresponding NumPy methods will be processed by the CPU as usual.  Data transfer happens through _streams_.  The null stream is the default.

CuPy provides several packages.  In this example we show its FFT implementation.
{{% code-download file="/courses/python-high-performance/codes/cupy_example.py" lang="python" %}}

**Exercise**
Providing enough work to fill the GPU's threads is critical to overcoming the overhead of copying data.  

Reduce the length of the array in the example to 10000, then increase it until you find a size for which the GPU is faster.

## PyCUDA

CUDA is a package from NVIDIA that enables access to the GPU through the C programming language.  With appropriate bindings it can be called from other languages.  [PyCUDA](https://documen.tician.de/pycuda/) is a package that implements CUDA bindings to Python.

Like CuPy, it is available through conda-forge.
```bash
conda install -c conda-forge pycuda
```

On Linux the PATH variable must include the location of the `nvcc` compiler. If you have your own Linux workstation you must first locate nvcc. It should be in the folder indicated by the CUDA_PATH variable, with a "bin" appended.
```bash
ls $CUDA_PATH/bin
```
Then add this location to your path, e.g.
```bash
export PATH=$CUDA_PATH/bin:$PATH
```

**Example**

This script is copied directly from PyCUDA's [examples](https://github.com/berlinguyinca/pycuda/tree/master/examples).
{{% code-download file="/courses/python-high-performance/codes/pycuda_example.py" lang="python" %}}

Much as we saw when discussing using [compiled code](/courses/python-high-performance/compiled_code), we must define our function in C style.  This block of code to be executed on the device is called a _kernel_.  PyCUDA compiles the kernel, uses its interface with NumPy to allocate memory on the device, copy the Ndarrays, carry out the computation, then copy the result from the device to the `dest` array.

Notice that the Ndarrays are declared to be `dtype=float32`.  Very few GPUs support double-precision (`float` or `float64` in Python) hardware, so using doubles may be slow as the computations must be done in software on the device.

## Numba

When used for GPU acceleration, the package relies on a conda package `cudatoolkit`.
```bash
conda install cudatoolkit
```
If you must use pip, you must also install the [NVIDIA CUDA SDK](https://numba.readthedocs.io/en/stable/user/installing.html).

Numba can be used with PyCUDA so adding it to the PyCUDA environment, which should already contain cudatoolkit, might be advisable. This example is from the PyCUDA [tutorial](https://github.com/berlinguyinca/pycuda/blob/master/doc/source/tutorial.rst).
{{% code-download file="/courses/python-high-performance/codes/pycuda_numba.py" lang="python" %}}

### Numba Vectorization

Numba CUDA can "vectorize" a universal function (ufunc) by compiling it and running it on the GPU.  Vectorization is implemented through a decorator.

For best performance, the signature of the function arguments must be specified.  If more than one type should be supported, all must be passed as a list.

**Example**

From the Numba documentation:
{{% code-download file="/courses/python-high-performance/codes/numba_vectorize.py" lang="python" %}}

The run may emit a warning about underutilization:
```no-highlight
Grid size (1) < 2 * SM count (40) will likely result in GPU under utilization due to low occupancy.
```
This is because efficient use of a GPU requires that the device threads be as filled as possible.  If too many threads are idle, GPU code can be slower than the equivalent CPU code.

Another requirement for efficient GPU utilization is memory management.  Using NumPy arrays will result in copying from the host memory to the device memory.
Calling a ufunc with NumPy arrays can result in a large amount of copying of data back and forth between host and device.
We can instead declare arrays that will be set up in the GPU memory.  The `todevice` method will copy the host array to the device array.  We can also declare an output array on the device for the result.  When we are done with our calculations, we explicitly copy the result back to the host memory.
{{% code-download file="/courses/python-high-performance/codes/numba_vectorize_todevice.py" lang="python" %}}

If you time these two scripts, you may see a small speedup even for this relatively low-efficiency problem when copying is minimized.  Of course, we should aim for a large amount of work on the device arrays before we return the output to the host, and we should strive to make the problem large enough to fill the GPU's threads.

### Numba GPU Kernels

Numba has a `cuda.jit` decorator that can be used like the `jit` equivalent, but defines a CUDA kernel. Some accommodations for the CUDA programming model must be made.  For the best performance, the number of threads on the hardware must be divided into _thread blocks_ and the optimal number may depend on the device. Similarly to PyCUDA, Numba over CUDA makes use of NumPy arrays.

**Example**

This example of a matrix-multiplication kernel is taken from the Numba CUDA [documentation](https://numba.readthedocs.io/en/stable/cuda/).

{{% code-download file="/courses/python-high-performance/codes/numba_cuda_example.py" lang="python" %}}

## RAPIDS

RAPIDS is a set of libraries released by NVIDIA for its GPU architectures (Pascal or later).  It builds upon CuPY and introduces a GPU DataFrame `cuDF`, and a package `cuML` that mostly replicates `scikit-learn`.
See our [RAPIDS workshop](/workshops/rapids) to learn more about using RAPIDS.
