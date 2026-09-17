---
title: GPU Architecture
date: 2026-07-22T17:26:29Z
type: book 
weight: 4010
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

GPUs were designed for real-time rendering of complex and realistic graphical scenes. Their architecture reflects the choices made to achieve this functionality. Rather than the few to tens of general-purpose and powerful cores of a CPU, a GPU contains many SM (streaming multiprocessing) units.  Each SM consists of several GPU cores, with each core capable of handling hundreds or thousands of threads but over a limited instruction set.

Most GPUs, particularly from NVIDIA, are **SIMT** devices.  SIMT (Single Instruction Multiple Threads) is a type of SIMD in which a central control unit broadcasts an instruction to multiple processing units (PUs), each of which synchronously and independently executes the instruction. Each processing unit has its own set of data in private memory and its own registers, but not its own program counter (a register which keeps track of the location in a program).  This type of device has been called an _array processor_, where "array" refers to an array of PUs, not a mathematical array.  But SIMT devices can be very fast at mathematical operations involving linear algebra, which is important for both graphics and machine learning.

In CUDA terminology, a base group of threads is a _warp_. Each SM has some number (depending on architecture) of _warp schedulers_. Thread warps are grouped into _blocks_, with each block scheduled on the same SM.  All threads in a warp execute the same code in parallel. While in principle the number of threads in a warp could depend on hardware, in practice it is 32. The warp can be regarded as analogous to a cache line in a CPU, and like the cache line, most programmers do not need to deal with its exact size but should keep its existence in mind to optimize code.

As an example, an NVIDIA H100 has 132 SMs, each with four warp schedulers for a total of 128 threads per SM or 16896 threads per board. These threads are executed in parallel.

SMs also support concurrent threading with context switching. Each SM on the example H100 can manage up to 2048 concurrent threads split into 64 warps.  GPU cores' context switching is much faster than on a CPU, typically up to 1000 times faster.

Each GPU core has its own set of private registers. The SM also contains an L1 cache shared by the cores.  These memory units are constructed from a very fast type called SRAM (static RAM). The SMs share L2 cache. Newer NVIDIA architectures also have a shared L3 cache. As is the case for CPUs, the cache memories are smaller and faster than the main memory but are comparatively limited in size. The SMs also share a main memory consisting of DRAM (dynamic RAM) similar to that used for CPU main memory, but for a GPU usually called VRAM (video RAM).

Most GPUs do not provide hardware support for double-precision floating-point numbers, so computations that require that precision must be modified or carried out on the CPU.

{{< figure src="/courses/parallel-computing-introduction/img/GPU_schematic.png" alt="Schematic illustration of GPU architecture consisting of SMs and a memory hierarchy." caption="Schematic illustration of a simplified GPU architecture. Red represents local registers on each GPU core, violet is L1 cache, L2 and L3 are labeled, and VRAM is yellow." >}}

