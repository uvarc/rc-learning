---
title: CPU Allocation - Fine-Tuning
date: "2025-02-23T00:00:00"
type: docs 
weight: 2950
menu: 
    llms-hpc:
      parent: Fine-Tuning
---


__CPU memory__
  * __Interactive Partition:__ The interactive partition requests 6GB RAM per core requested.
  * __Standard Partition:__ The standard partition requests 9GB RAM per core requested (no GPU).
  * __GPU partition:__  You can select the amount of CPU RAM.
  
You should have enough RAM to comfortably work with your GPU.
In other words, request at least as much RAM as the GPU you select.
If you are using a large dataset and/or want to do extensive preprocessing, more RAM is probably helpful.

__CPU cores__

Use multiple cores - especially if you are using a dataset from the Datasets package and a GPU (So that DataLoader can utilize multiple cores under the hood).

A good starting point is to use 8 cores.

---

Notes: 

Check your resource usage with the GPU Dashboard, or use `seff` for completed jobs and `sstat` for running jobs.

It may be the case that even if CPU Efficiency is a low percentage, you need all of the requested CPU cores for a specific part of the code, e.g., data preprocessing.
In this case, request the number of CPU cores that you need for the compute intensive part of the code.


