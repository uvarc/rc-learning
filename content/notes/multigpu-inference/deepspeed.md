---
title: DeepSpeed
date: 2025-07-08-20:40:54Z
type: docs 
weight: 1600
menu: 
    multigpu-inference:
     name: DeepSpeed
---

DeepSpeed is Microsoft’s open source that focuses on large-scale training and inference operation through efficient memory management for PyTorch.

Key components for inference are custom CUDA kernels for common LLM operations like attention and MLP and tensor parallelism for efficient memory usage and low latency.

DeepSpeed also has several architecture and quantization specific optimizations.

Upshot: DeepSpeed allows you to load larger models that would not ordinarily fit within GPU memory, though your mileage may vary.



