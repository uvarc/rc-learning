---
title: Advantages of DeepSpeed
date: 2025-07-08T20:40:54Z
type: docs 
weight: 1750
menu: 
    multigpu-inference:
        parent: DeepSpeed
---

You should use DeepSpeed if you need to either perform large-scale inference (hundreds+ GPUs), you need fast service, or every MB in GPU memory counts.

Remark: the maximum number of GPUs you can request for a UVA HPC job is 32 (max number of nodes is 4).

The inference latency reductions can matter if you only have X amount of time for experiments.

DeepSpeed also offers very efficient large-scale training parallelism.

For most users, Accelerate is better due to ease of use. DeepSpeed does improve inference, but is mainly used for training.  If serving an LLM, vLLM is a good choice.

Source: https://www.rc.virginia.edu/userinfo/hpc/#job-queues




