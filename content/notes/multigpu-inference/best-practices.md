---
title: Best Practices
date: 2025-07-08-20:40:54Z
type: docs 
weight: 1950
menu: 
    multigpu-inference:
        name: Best Practices
---


**Plan first.**

Calculate how much GPU memory you will need to run inference.

Check the GPU queue to see which GPUs are available.

Determine how many GPUs and which types (A40, A100, etc.) you will need.

**Need fast GPU to GPU communication? Use the BasePOD.**

If you would like to use fewer GPUs, you can try using a quantized model, an LLM with a lower precision and/or a smaller number of parameters, etc. and see if performance is acceptable.

Use a well-established framework such as Accelerate.  If you need extra gains, you can try DeepSpeed or vLLM.



