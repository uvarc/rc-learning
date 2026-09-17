---
title: OpenMP and OpenACC
date: 2026-08-31T17:26:29Z
type: book 
weight: 4030
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

OpenACC was developed by the Portland Group Inc. (PGI), a compiler vendor, to provide a simpler interface for programming GPUs when CUDA was the only general-purpose option. It was a directives-based model much like OpenMP, before OpenMP supported devices, and it is very similar.  NVIDIA bought PGI and renamed their compiler suite the NVIDIA HPC compilers. OpenACC has been released as an open-source project and is available in NVIDIA HPC SDK (software development kit) (nvhpc) and GCC, along with some less commonly used suites. The best support is generally in nvhpc but it is only available for Linux. GCC support is more cross-platform but generally lags behind nvhpc.

Starting with Version 4.0, OpenMP has been updated and extended beyond the multicore model we have examined to include SIMD/SIMT devices. Implementation is the responsibility of the compiler vendor, and some compilers support newer versions of the standard better than others.  

The choice to use a directives-based programming model is based on a balance between performance and programmer time.  Low-level programming language extensions like CUDA provide the best performance, but in addition to being more difficult to learn, they often require hand-tuning which can be hardware dependent. The languages may not be portable (CUDA is NVIDIA-only, Metal is Apple-only). Comparably easier programming, portability, and at least some degree of hardware independence can more than make up for the reduced execution efficiency for many purposes. In the most compute-intensive tasks such as machine-learning applications, manual programming for maximum efficiency can be particularly important, but many applications do not require this.

The choice of OpenACC or OpenMP depends on the goals.

OpenACC is
* More tuned to GPUs, specifically NVIDIA hardware
* Particularly optimized for NVIDIA hardware
* Well suited to applications with large data access

OpenMP is
* Similar syntax for both multithreaded CPUs and SIMT hardware
* Supported by more compilers than OpenACC
* Good for heterogeneous systems where both host and device computations are important

