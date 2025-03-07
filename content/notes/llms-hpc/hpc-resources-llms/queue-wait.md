---
title: Wait Time in the Queue
date: 2025-02-23-19:06:23Z
type: docs 
weight: 1150
menu: 
    llms-hpc:
        parent: HPC Resources for LLMs
---


You may not need to request an A100 GPU!

Requesting an A100 may mean you wait in the queue for a much longer time than using another GPU. This could give you a slower overall time (wait time + execution time) than if you had used another GPU.

{{< figure src=/notes/llms-hpc/img/LLMS_on_HPC_4.png width=90% height=90% >}}

**When you request memory for HPC, that is CPU memory.**

If you request a GPU, you will receive all of the GPU memory.

