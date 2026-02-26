---
title: Multi-GPU Strategies 
date: 2025-07-08T20:40:54Z
type: docs 
weight: 1050
menu: 
    multigpu-inference:
        name: Multi-GPU Strategies 
---

This section will outline the parallelism strategies for multi-GPUs. 

__Data Parallelism__ : data is split across GPUs.

__Model Parallelism__ : model is split across GPUs. There are two types: Pipeline Parallelism and Tensor Parallelism.

{{< table >}}
| Parallelism Type | What is Split? | Use Cases |
| :-: | :-: | :-: |
| Data Parallelism | Input data | Long prompts/inputs |
| Pipeline Parallelism | Model layers | LLM exceeds single GPU memory: LLM is deep but not too wide |
| Tensor Parallelism | Inside model layers (tensors) | LLM exceeds single GPU memory: LLM layers are too large for one device |
{{< /table >}}




