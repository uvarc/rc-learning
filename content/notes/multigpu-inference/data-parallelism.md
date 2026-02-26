---
title: Data Parallelism
date: 2025-07-08T20:40:54Z
type: docs 
weight: 1150
menu: 
    multigpu-inference:
        parent: Multi-GPU Strategies 
---

{{< figure src=/notes/multigpu-inference/img/Multi_GPU_LLM_Inference_26.png width=50% height=50% >}}

In data parallelism, each GPU contains a full copy of the model.

Data is split across GPUs, and inference is performed on GPUs in parallel. This parallelism provides inference speed up.



Source: [https://colossalai.org/docs/concepts/paradigms_of_parallelism/#data-parallel](https://colossalai.org/docs/concepts/paradigms_of_parallelism/#data-parallel)

