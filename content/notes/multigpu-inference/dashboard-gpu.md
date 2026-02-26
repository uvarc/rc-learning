---
title: GPU Dashboard 
date: 2025-07-08T20:40:54Z
type: docs 
weight: 800
menu: 
    multigpu-inference:
        parent: UVA HPC
---


{{< figure src=/notes/multigpu-inference/img/Multi_GPU_LLM_Inference_19.png width=40% height=40% >}}

The GPU dashboard in OOD JupyterLab includes GPU and CPU memory and utilization tracking in real time. This is helpful for GPU selection. 

### Memory Usage

__PyTorch__

Correct GPU memory usage will be reported.

__TensorFlow/__ __Keras__

By default, TF automatically allocates ALL of the GPU memory so GPU Dashboard may show that all (or almost all) of the GPU memory is being used.

To track the amount of GPU memory actually used, you can add these lines to your python script:
```bash
import os
os.environ['TF_FORCE_GPU_ALLOW_GROWTH'] = 'true'
```

More Info: [https://www.tensorflow.org/guide/gpu#limiting_gpu_memory_growth](https://www.tensorflow.org/guide/gpu#limiting_gpu_memory_growth)

>Homework for Keras users: try out GPU dashboard and see if it reports all of the GPU memory as used





