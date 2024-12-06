---
title: Accelerating applications with NVIDIA GPUs
date: 2024-11-16-20:28:40Z
type: docs 
weight: 300
menu: 
    matlab-parallel-programming:
---

## Why MATLAB with NVIDIA GPUs
  - Easy access to NVIDIA GPUs with 990+ GPU-enabled functions  
  - MATLAB users can work with NVIDIA GPUs without CUDA programming  
  - NVIDIA GPUs accelerate many applications like AI/Deep Learning  

## Graphics Processing Units (GPUs)
  - For graphics acceleration and scientific computing  
  - Many parallel processors  
  - Dedicated high-speed memory  

{{< figure src="/notes/matlab-parallel-programming/img/gpu-cores.png" width="300px" >}}

## GPU Requirements

*Note: Parallel Computing Toolbox requires NVIDIA GPUs. [Learn more here.](http://www.nvidia.com/object/cuda_gpus.html)*

| MATLAB Release | Required Compute Capability |
| :-: | :-: |
| MATLAB R2018a and later releases | 3.0 or greater |
| MATLAB R2014b - MATLAB R2017b | 2.0 or greater |
| MATLAB R2014a and earlier releases | 1.3 or greater |

## GPU Computing Paradigm
### NVIDIA CUDA-enabled GPUs

{{< figure src=/notes/matlab-parallel-programming/img/parallel-toolbox-to-gpus.png >}}

---

Our tools can both be used to speed up your calculation using multiple CPUs and by using GPUs.  

Although GPUs have hundreds of cores, we treat the GPU as a single  unit, and access directly from a MATLAB computation engine. For example, any single Worker can access the entire GPU.       

Expected speed-up varies with problem specifics as well as the avaialable hardware.

## Programming with GPUs

- Easiest to use: Parallel-enabled toolboxes
- Moderate ease of use and moderate control: Common programming constructs (`gpuArray`, `gather`)
- Greatest control: Advanced programming constructs (`spmd`, `arrayfun`, `CUDAKernel`, `mex`))

## Demo: Wave Equation
### Accelerating scientific computing in MATLAB with GPUs

* Objective: Solve 2<sup>nd</sup> order wave equation with spectral methods
* Approach:
  * Develop code for CPU
  * Modify the code to use GPUcomputing using gpuArray
  * Compare performance ofthe code using CPU and GPU

{{< figure src=/notes/matlab-parallel-programming/img/second-order-wave-equation-example.png >}}

---

## Speed-up using NVIDIA GPUs

{{< figure src=/notes/matlab-parallel-programming/img/speed-up-gpu.png >}}

* Ideal Problems
  * Massively Parallel and/or Vectorized operations
  * Computationally Intensive
* 500+ GPU-enabled MATLAB functions
* Simple programming constructs
  * `gpuArray`, `gather`

---

In the case of GPUs, it's slightly different.  
Ideal problems for GPU computing :

1. Massively parallel—The computations can be broken down into hundreds or thousands of independent units of work.  You will see the best performance all of the cores are kept busy, exploiting the inherent parallel nature of the GPU. 
2. Computationally intensive—The time spent on computation significantly exceeds the time spent on transferring data to and from GPU memory. Because a GPU is attached to the host CPU via the PCI Express bus, the memory access is slower than with a traditional CPU. This means that your overall computational speedup is limited by the amount of data transfer that occurs in your algorithm. 
3. Algorithm consists of supported functions

Our developers have written CUDA versions of key MATLAB and toolbox functions and presented them as overloaded  functions - We have over 500 GPU-enabled functions in MATLAB and a growing number of GPU-enabled functions in additional toolboxes as well.

The diagram pretty much sums up the easiest way to do GPU computing in MATLAB - Transfer/create data on the GPU using the “gpuArray”, run your function as you would normally - if the inputs are available on the GPU, we do the right thing and run on the GPU and then “gather” the data back to the CPU. This seamless support allows you to run the same code on both the CPU and the GPU.

## GPU Computing with Matlab

**Useful Links**

**[GPU Computing](https://www.mathworks.com/help/parallel-computing/gpu-computing.html?s_tid=CRUX_lftnav)**

**[GPU Computing in MATLAB](https://www.mathworks.com/help/parallel-computing/gpu-computing-in-matlab.html)**

**[MATLAB GPU Computing Support for NVIDIA CUDA-Enabled GPUs](https://www.mathworks.com/solutions/gpu-computing.html)**

**[Run MATLAB Functions on Multiple GPUs](https://www.mathworks.com/help/parallel-computing/run-matlab-functions-on-multiple-gpus.html)**

**[Boost MATLAB algorithms using NVIDIA GPUs](https://www.mathworks.com/videos/boost-matlab-algorithms-using-nvidia-gpus-1624894710218.html)**