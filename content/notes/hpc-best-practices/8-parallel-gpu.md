---
title: VIII - Parallel/GPU Computing
date: 2025-06-14-14:47:30Z
type: docs 
weight: 400
toc: true
menu: 
    hpc-best-practices:
---

__Why go parallel?__
* __Speed__ - Solve a problem more quickly
* __Scale__ - Solve a larger, more complex problem with higher fidelity
* __Throughput__ - Solve many (simple) problems more quickly

# Parallel Computing Myths

__Myth #1: Throwing more hardware at a problem will automatically reduce the time to solution.__

Parallel computing will only help if you have an application that has been written to take advantage of parallel hardware. And even if you do have a parallel code, there is an inherent limit on scalability. (Caveat - a high-throughput computing workload can use parallel computing to run many single-core/GPU instances of an application to achieve near perfect scaling.)

__Myth #2: You need to be a programmer or software developer to make use of parallel computing.__

Most users of parallel computers are not programmers. Instead, they use mature 3rd party software that had been developed elsewhere and made available to the community.

{{< figure src="/notes/hpc-best-practices/img/othercode.png" width=70% height=70% caption="Many of you will be using someone else's code(s)" >}}


# Processes and Threads

Threads and processes are both independent sequences of execution.
* __Process:__ instance of a program, with access to its own memory, state, and file descriptors (can open and close files)
    * Incur more overhead
    * Are more flexible - multiple processes can be run within a compute node or across multiple compute nodes (distributed memory)
* __Thread:__ lightweight entity that executes inside a process. Every process has at least one thread, and threads within a process can access shared memory.
    * Incur less overhead
    * Threaded codes can use less memory since threads within a process have access to the same data structures
    * Are less flexible - multiple threads associated with a process can only be run within a compute node (shared memory)

Online resources describing the differences between thread and processes tend to be geared towards computer scientists, but the following resources do a reasonable job of addressing the topic:
* https://stackoverflow.com/questions/200469/what-is-the-difference-between-a-process-and-a-thread (nerdier)
* https://www.educba.com/process-vs-thread/ (more informal)

__Processes and threads - why do I care?__

The type of parallelization will determine how-where you will run your code.
* Distributed-memory applications (multiple processes/instances of a program) can be run on one or more nodes
* Shared-memory (threaded) applications should be run on a single node
* Hybrid applications can be run on one or more nodes, but should consider the balance between threads and processes (discussed later)
* In all cases, may need to consider how processes and threads are mapped and bound to cores

In addition, being aware of threads and processes will help you to understand how your code is utilizing the hardware and identify common problems.


# Message Passing Interface

MPI is a standard for parallelizing C, C++, and Fortran code to run on distributed memory (multiple compute node) systems. While not officially adopted by any major standard bodies, it has become the *de facto* standard (i.e., almost everyone uses it).

There are multiple open-source implementations, including OpenMPI, MVAPICH, and MPICH along with vendor-supported versions. MPI applications can be run within a shared-memory node. All widely-used MPI implementations are optimized to take advantage of the faster intranode communications. MPI is portable and can be used anywhere. Although MPI is often synonymous with distributed memory parallelization, other options are gaining adoption (Charm++, UPC, X10).

# OpenMP

OpenMP is an application programming interface (API) for shared-memory (within a node) parallel programming in C, C++, and Fortran.

OpenMP provides a collection of compiler directives, library routines, and environment variables. OpenMP is supported by all major compilers, including IBM, Intel, GCC, PGI, and AMD Optimizing C/C++ Compiler (AOCC). OpenMP is portable and can be used anywhere. Although OpenMP is often synonymous with shared-memory parallelization, there are other options: Cilk, POSIX threads (pthreads) and specialized libraries for Python, R, and other programming languages.

{{< figure src="/notes/hpc-best-practices/img/mpi-openmp.png" width=70% height=70% >}}


# Amdahl's Law and Limits on Scalability

Amdahl's law describes the absolute limit on the speedup of a code as a function of the proportion of the code that can be parallelized and the number of processors. This is the most fundamental law of parallel computing!

{{< figure src="/notes/hpc-best-practices/img/amdahl1.png" width=70% height=70% >}}

P is the fraction of the code that can be parallelized, S is the fraction of the code that must be run sequentially (S=1-P), and N is the number of processors.

In the limit as the number of processors goes to infinity, the theoretical speedup depends only on the proportion of the parallel content.

{{< figure src="/notes/hpc-best-practices/img/amdahl2.png" width=70% height=70% >}}

That doesn't look so bad, but as you'll see next, it doesn't take much serial content to quickly impact the speedup.

{{< figure src="/notes/hpc-best-practices/img/amdahl3.png" width=70% height=70% >}}


# Other Limits on Scalability

Amdahl's law sets a __theoretical upper limit__ on speedup, but there are other factors that affect scalability:
* Communications overhead
* Problem size
* Uneven load balancing

In real-life applications that involve communications, synchronization (all threads or processes must complete their work before proceeding), or irregular problems (non-cartesian grids), the speedup can be much less than predicted by Amdahl's law.


# Running Parallel Applications

So far, we've covered the basic concepts of parallel computing - hardware, threads, processes, hybrid applications, implementing parallelization (MPI and OpenMP), Amdahl's law, and other factors that affect scalability.

Theory and background are great, but how do we know how many CPU's GPUs to use when running our parallel application? The only way to definitively answer this question is to perform a __scaling study__ where a __representative problem__ is run on different number of processors. A representative problem is one with the same size (grid dimensions; number of particles, images, genomes, etc.) and complexity (e.g., level of theory, type of analysis, physics, etc.) as the research problems you want to solve.


# Presenting Scaling Results

Plotting the same data on log axes gives a lot more insight. Note the different scales for the left axes on the two plots. Including a line showing linear scale and plotting the parallel efficiency on the right axis adds even more value.

{{< figure src="/notes/hpc-best-practices/img/scale1.png" width=70% height=70% >}}

# Where should I be on the scaling curve?

If your work is not particularly sensitive to the time to complete a single run, consider using a CPU/GPU count at or very close to 100% efficiency, even if that means running on a single core. This especially makes sense for parameter sweep workloads where the same calculation is run many times with different sets of inputs.

{{< figure src="/notes/hpc-best-practices/img/scale2.png" width=70% height=70% >}}

Go a little further out on the scaling curve if the job would take an unreasonably long time at lower core counts or if a shorter time to solution helps you make progress in your research. If the code does not have checkpoint-restart capabilities and the run time would exceed queue limits, you'll have no choice but to run at higher core counts.

{{< figure src="/notes/hpc-best-practices/img/scale3.png" width=70% height=70% >}}

If the time to solution is absolutely critical, it's okay to run at lower efficiency. Examples might include calculations that need to run on a regular schedule (data collected during day must be processed overnight) or severe weather forecasting.

{{< figure src="/notes/hpc-best-practices/img/scale4.png" width=70% height=70% >}}


# Parallel Computing Summary

* Parallel computing is for everyone who wants to accomplish more research and solve more challenging problems
* You don't need to be a programmer, but you do need to know some of the fundamentals to effectively use parallel computers
* Processes are instances of programs; threads run within a process and access shared data; MPI and OpenMP are used to parallelize codes
* Amdahl's law imposes an *upper* limit on scalability, but there are other factors that impact scalability (load imbalance, communications overhead)
* Know how to display your scaling data and choose core counts

For code used to generate some figures in these notes, see https://github.com/sinkovit/Parallel-concepts


# GPU vs CPU Architecture

__CPU:__
* Few processing cores with sophisticated hardware
* Multi-level caching
* Prefetching
* Branch prediction

__GPU:__
* Thousands of simplistic compute cores (packaged into a few multiprocessors)
* Operate in lock-step
* Vectorized loads/stores to memory
* Need to manage memory hierarchy


# GPU Accelerated Software

__Examples can be found from virtually any field:__
* Chemistry
* Life sciences
* Bioinformatics
* Astrophysics
* Finance
* Medical imaging
* Natural language processing
* Social sciences
* Weather and climate
* Computational fluid dynamics
* Machine learning

Exhaustive list of software can be found here: https://www.nvidia.com/en-us/data-center/gpu-accelerated-applications/


# GPU Accelerated Libraries

__Ease of use:__
* GPU acceleration without in-depth knowledge of GPU programming
__"Drop-in"__
* Many GPU accelerated libraries follow standard APIs
* Minimal code changes required
__Quality__
* High-quality implementations of functions encountered in a broad range of applications
__Performance__
* Libraries are tuned by experts

Use libraries if you can - do not write your own matrix multiplication.

{{< figure src="/notes/hpc-best-practices/img/gpuex.png" width=70% height=70% caption="Examples of GPU accelerated libraries" >}}

See https://developer.nvidia.com/gpu-accelerated-libraries for more info.


# Numerical Computing in Python

{{< figure src="/notes/hpc-best-practices/img/numpy.png" width=70% height=70% >}}

__NumPy:__
* Mathematical focus
* Operates on arrays of data
    * *ndarray*, holds data of same type
* Many years of development
* Highly tuned for CPUs

{{< figure src="/notes/hpc-best-practices/img/cupy.png" width=70% height=70% >}}

__CuPy:__
* NumPy-like interface
* Trivially port code to GPU
* Copy data to GPU
    * CuPy *ndarray*
* Data interoperability with DL frameworks, RAPIDS, and Numba
* Uses high tuned NVIDIA libraries
* Can write custom CUDA functions

## CuPy: a NumPy like interface to GPU-acceleration ND-Array operations

__Before (with NumPy):__
```python
import numpy as np

size = 4096
A = np.random.randn(size,size)

Q, R = np.lingalg.qr(A)
```

__After (with CuPy):__
```python
import cupy as np

size = 4096
A = np.random.randn(size,size)

Q, R = np.lingalg.qr(A)
```

{{< figure src="/notes/hpc-best-practices/img/speedup.png" width=70% height=70% >}}


# Processing Flow

{{< figure src="/notes/hpc-best-practices/img/flow.png" width=70% height=70% >}}
