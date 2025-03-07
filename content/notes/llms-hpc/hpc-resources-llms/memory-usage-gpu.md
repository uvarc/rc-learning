---
title: GPU Memory Usage
date: 2025-02-23-19:06:23Z
type: docs 
weight: 1300
menu: 
    llms-hpc:
        parent: HPC Resources for LLMs
---

__PyTorch__

Correct GPU memory usage will be reported by GPU Dashboard.

__TensorFlow/Keras__ 

By default, TF automatically allocates ALL of the GPU memory so GPU Dashboard may show that all (or almost all) of the GPU memory is being used.

To track the amount of GPU memory actually used, you can add these lines to your python script:

```python
import os

os.environ['TF_FORCE_GPU_ALLOW_GROWTH'] = 'true'

```

[More Info](https://www.tensorflow.org/guide/gpu#limiting_gpu_memory_growth)

Homework for Keras users: try out GPU dashboard and see if it reports all of the GPU memory as used.


### Resource Allocation for LLMs


Resource needs will vary based on LLM use (inference, fine-tuning, etc.)

We will cover good starting choices in the Inference and Fine-Tuning sections of today’s workshop.
