---
title: Hardware Overview
date: 2024-06-28T01:51:43Z
type: docs 
weight: 250
toc: true
menu: 
    deep-learning-hpc:
---


## HPC Overview

{{< figure src=/notes/deep-learning-hpc/img/RC_HPC.png caption="Source: https://www.rc.virginia.edu/userinfo/hpc/#hardware-configuration" width=55% height=55% >}}


### Partitions: 

* Standard (no GPUs)
* Largemem (no GPUs)
* __GPU__ (compute node with NVIDIA GPU(s))
* __Interactive__  (compute node with NVIDIA GPU(s))

Visit our [documentation](https://www.rc.virginia.edu/userinfo/hpc/#hardware-configuration) on HPC hardware configuration for more information.


## GPU General Overview

* Graphics Processing Units (GPUs), originally developed for accelerating graphics rendering, can dramatically speed up any simple but highly parallel computational processes (General Purpose GPU).


### CPU vs. GPU:
{{< table >}}
| CPU | GPU |
| --- | --- |
| Several Cores (10<sup>0-1</sup>)  | Several Cores (10<sup>3-4</sup>) |
| Low Latency | High Throughput |
| Generic Workload (Complex & Serial Processing) | Specific Workload (Simple & Highly Parallel) |
| Up to 1.5 TB / node on Rivanna   | Up to 80 GB /device on Rivanna      |
{{< /table >}}


* Integrated GPU vs Discrete GPUs:
  * Integrated GPUs are used mostly for graphics rendering and gaming
  * Dedicated GPUs are designed for intensive computations


#### Vendors and Types:

* NVIDIA, AMD, Intel
* Datacenter : K80, P100, V100, A100, H100 (NVIDIA); MI300A, MI300X (AMD)
* Workstations: A6000, Quadro (NVIDIA)
* Gaming: GeForce RTX 20xx, 30xx, 40xx (NVIDA), Radeon (AMD)
* Laptops and desktops: GeForce (NVIDIA), Radeon (AMD), Iris (Intel)


#### Programming GPUs

Several libraries and programming models have been developed to program GPUs.

* CUDA 
  * CUDA is a parallel computation platform, developed by NVIDIA, for general-purpose programming on NVIDIA hardware.
* HIP
  * HIP is a programming interface from AMD that allows developers to target either NVIDIA or AMD hardware.
* OpenCL 
  * OpenCL is a more general parallel computing platform, developed by Apple. It allows software to access CPUs, GPUs, FPGAs, and other devices. 
* SYCL
  * SYCL started as an outgrowth of OpenCL but is now independent of it.
* Kokkos
  * Kokkos is another programming model that attempts to be device-independent. It can target multicore programming (OpenMP), CUDA, HIP, and SYCL.

Most of these programming paradigms can be used from Python, but nearly all machine learning/deep learning packages are based on CUDA and  will only work with NVIDIA GPUs.


