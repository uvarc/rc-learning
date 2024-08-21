---
title: Resource Allocation and Helpful Tools
date: 2024-06-28-01:51:43Z
type: docs 
weight: 400
toc: true
menu: 
    deep-learning-hpc:
---


## Computations on the CPU or GPU
{{< table >}}
| Task  | CPU or GPU |
| ---   | --- |
| Data Preprocessing    | Either, but probably CPU |
| Deep Learning Training   | GPU |
| Deep Learning Inference  | Either, but probably GPU |
{{< /table >}}


When you request memory for Rivanna, that is __CPU memory__. If you request a GPU, you will receive all of the GPU memory.
