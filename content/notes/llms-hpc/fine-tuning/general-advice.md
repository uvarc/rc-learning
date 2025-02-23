---
title: General Advice
date: 2025-02-23-19:06:23Z
type: docs 
weight: 3050
menu: 
    llms-hpc:  
        parent: Fine-Tuning
---


If you are learning about LLMs and doing tutorials, choose a small LLM.  The GPUs in the Interactive partition are probably ok to use.

You can leave the GPU choice as default on the GPU partition and work on whichever GPU you get or choose a GPU with a smaller amount of memory first.

Fine-tune your model for one epoch and monitor the GPU memory usage using GPU Dashboard.

If you are getting OOM (out of memory) GPU errors, try lowering the batch size.

There are other advanced techniques to reduce the amount of memory used in fine-tuning.

