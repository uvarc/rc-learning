---
title: Accelerators
date: 2026-07-22T17:26:29Z
type: book 
weight: 4000
menu: 
    parallel_programming:
        parent: Accelerator Programming
---

Accelerators are specialized hardware to which data-intensive and/or compute-intensive tasks can be offloaded.  They included GPUs (graphics processing units), TPUs (tensor processing units), FPGAs (field-programmable gate arrays) and ASICs (application-specific integrated circuits).

By far the most widely used accelerator is the GPU. As its full name suggests, it was developed to handle the computations required for high-speed graphical processing tasks for rendering video.  They were and many still are programmed with graphics-oriented libraries including DirectX\&trade; and OpenGL. The first attempts to harness the massively-parallel capabilities of GPUs were made by programmers who repurposed OpenGL to "trick" the GPU into doing other types of computations. NVIDIA, the dominant manufacturer of GPUs, recognized that a simpler approach was needed and developed CUDA\$&trade; (Compute Unified Device Architecture), a library that allows relatively simple (for experienced C programmers) programming of their devices.  After its official release in 2007 CUDA became dominant for general-purpose computing on GPUs, resulting in the architecture of the GPU moving toward supporting CUDA and general-purpose computation at least as well as the specialized, mostly linear-algebra based, mathematics of graphics. These newer architectures are sometimes called GPGPUs (general-purpose graphics processors), a rather contradictory name but a good description of the functionality.

As Artificial Intelligence (AI) and Large Language Model (LLM) applications have increasingly dominated GPGPU applications, TPUs were developed at Google as an alternative. They are a type of ASIC designed specifically for neural-network machine learning without the legacy hardware for graphics or more general computing. Nevertheless, GPUs are still useful and in some applications, GPUs are used for training and TPUs for inference. Google has ported its own Tensorflow ML (machine learning) model, as well as Torch and JAX (a Python library for array computations), to their TPUs.

We will focus on GPUs as they are most widely used and available, but many libraries attempt to abstract the specifics of the device away, leaving the vendor to implement the underlying drivers and low-level libraries.
