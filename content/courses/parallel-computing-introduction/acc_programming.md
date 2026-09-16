---
title: GPU Programming Model
date: 2026-07-22T17:26:29Z
type: book 
weight: 4020
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

The accelerator is a type of _coprocessor_, a unit separate from the CPU that carries out certain computations.  Hence it is necessary to transfer data from main CPU memory to the accelerator memory.

We will use CUDA terminology in this discussion since it is the most widely used lower-level programming library for GPUs.

The CPU is called the _host_ and the accelerator is the _device_. Code to be executed on the device is the _device code_.  A function executed on the device is called a _kernel_.  The host code _launches_ a kernel by transferring the data to the device. The kernel is responsible for launching threads.

From the programming perspective, thread blocks are organized into _grids_ which can be 1, 2, or 3 dimensional.  Kernels are launched with an _execution configuration_ specifying the grid layout.  It is not always necessary for most programmers to be concerned about how grids map to SMs, but SM assignments and other hardware details can optionally be included in the execution configuration.

As already mentioned, all threads in a thread block run on the same SM, but the independence of the SMs means that blocks must not have dependencies on other blocks.

The transfer of data between the host and device is the major bottleneck in GPU programming and should be minimized.

{{< figure src="/courses/parallel-computing-introduction/img/GPU_programming_model.png" alt="GPU programming requires transfer of data from the host to the device, and from device to host." caption="The data for the computation is transferred between host DRAM and device VRAM and the result is returned from VRAM to DRAM." >}}

## Programming GPGPUs

GPUs are **SIMD** (single-instruction multiple data) systems. They are programmed with a threading model. The limited instruction set and SIMD model means that GPUs are typically programmed by launching multiple kernels within a program.  (Some newer NVIDIA models have the ability to handle multiple streams of instruction, but this is only for some architectures and only available in CUDA.)  

Programming systems can be broken into three basic categories.  Below are some examples of each type.

### Low-level libraries/languages

1. [CUDA](https://docs.nvidia.com/cuda/doc/index.html). NVIDIA only.
2. [HIP/HPCC/ROCM](https://rocm.docs.amd.com/projects/HIP/en/latest/index.html)  AMD product intended to be generic but aimed at their hardware.
3. [SYCL](https://www.khronos.org/sycl/) C++ library intended to abstract the device.  Successor to Apple's OpenCL.
4. [Metal](https://developer.apple.com/metal/) Apple's library for their hardware. Aimed at machine learning but can also handle graphics and gaming.

### Intermediate libraries/directives

1. [OpenMP](https://www.openmp.org/).  OpenMP has been extended considerably to enable it to work with devices.
2. [OpenACC](https://www.openacc.org/). NVIDIA directives-based library.

### High-level language support

1. [CUDA Python](https://developer.nvidia.com/how-to-cuda-python) and Numba.
2. [JuliaGPU](https://juliagpu.org/) for the Julia language.
3. [torch.compile](https://docs.pytorch.org/tutorials/intermediate/torch_compile_tutorial.html) Interface for kernel optimization for the Torch machine-learning system.

