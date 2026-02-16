---
title: CPU Resource Allocation
date: 2025-07-08T20:40:54Z
type: docs 
weight: 1000
menu: 
    multigpu-inference:
        parent: UVA HPC
---

For LLM inference, CPU memory and cores affect offloading and data preprocessing.

__CPU memory__

Have enough RAM to fit your LLM. Libraries such as **Accelerate** will offload computations to the CPU after GPUs are full. 

__CPU cores__

Use enough cores for any data preprocessing. Use a GPU whenever possible to optimize speed and efficiency. 

Check your resource usage using GPU Dashboard, `seff` (completed jobs), or `sstat` (running jobs).
It may be the case that even if CPU Efficiency is a low percentage, you need all of the requested CPU cores for a specific part of the code, e.g., data preprocessing.
In this case, request the number of CPU cores that you need for the compute intensive part of the code.

Source: [https://timdettmers.com/2018/12/16/deep-learning-hardware-guide/](https://timdettmers.com/2018/12/16/deep-learning-hardware-guide/)







