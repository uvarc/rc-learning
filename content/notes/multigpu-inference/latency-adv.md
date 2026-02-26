---
title: Transformer-specific Latency Advantages
date: 2025-07-08T20:40:54Z
type: docs 
weight: 1700
menu: 
    multigpu-inference:
        parent: DeepSpeed
---

{{< figure src=/notes/multigpu-inference/img/Multi_GPU_LLM_Inference_38.jpg width=90% height=90% caption="Transformer module latency reductions per module." >}}

The diagram above highlights key transformer components and shows how latency can be reduced in each part using optimized operations, such as fused QKV and faster bias-add kernels.



