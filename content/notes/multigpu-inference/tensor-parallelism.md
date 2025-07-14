---
title: Tensor Parallelism (Intra-Layer)
date: 2025-07-08-20:40:54Z
type: docs 
weight: 1250
menu: 
    multigpu-inference:
      parent: Multi-GPU Strategies 
---


{{< figure src=/notes/multigpu-inference/img/Multi_GPU_LLM_Inference_29.png width=60% height=60% caption="Matrix Multiplication Example." >}}

Tensor parallelism (intra-layer) splits tensor computations across GPUs. Each GPU contains part of the tensor. GPUs compute on their part in parallel. THis is good for transformer layers. However, it requires frequent communication/fast interconnect (e.g. NVLink, which UVA HPC has in the BasePOD).


Source: [https://colossalai.org/docs/concepts/paradigms_of_parallelism/#tensor-parallel](https://colossalai.org/docs/concepts/paradigms_of_parallelism/#tensor-parallel)

