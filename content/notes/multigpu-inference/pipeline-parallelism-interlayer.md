---
title: Pipeline Parallelism (Inter-Layer)
date: 2025-07-08T20:40:54Z
type: docs 
weight: 1200
menu: 
    multigpu-inference:
      parent: Multi-GPU Strategies 
---

{{< figure src=/notes/multigpu-inference/img/parallelism_pipeline.png width=70% height=70% >}}

In inter-layer pipeline parallelism, each GPU contains a different model stage (1+ layers).

GPUs compute on batches of data in parallel but must wait for previous stage to complete.

{{< figure src=/notes/multigpu-inference/img/bubble-parallelism.png width=80% height=80% >}}

The advantages of inter-layer pipeline parallelism are that it reduces per-GPU memory use and improves inference throughput. The disadvantage of inter-layer pipeline parallelism is that it adds inference latency.


Source: [https://colossalai.org/docs/concepts/paradigms_of_parallelism/#pipeline-parallel](https://colossalai.org/docs/concepts/paradigms_of_parallelism/#pipeline-parallel), [https://arxiv.org/abs/1811.06965](https://arxiv.org/abs/1811.06965)

