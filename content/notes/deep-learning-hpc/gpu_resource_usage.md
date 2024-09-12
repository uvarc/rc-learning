---
title: GPU Resource Usage
date: 2024-06-28-01:51:43Z
type: docs 
weight: 650
toc: true
menu: 
    deep-learning-hpc:
        parent: Resource Allocation and Helpful Tools
---

## NVIDIA GPU Resource Usage
`nvidia-smi`  will report GPU utilization and memory usage for NVIDIA GPUs.
* _GPU Utilization_ refers to the percentage of time that at least one kernel was running on the GPU.

`watch -n 1 nvidia-smi` will update the display every second.

{{< figure src=/notes/deep-learning-hpc/img/nvidia.png caption="Source: [https://medium.com/analytics-vidhya/explained-output-of-nvidia-smi-utility-fc4fbee3b124](https://medium.com/analytics-vidhya/explained-output-of-nvidia-smi-utility-fc4fbee3b124) and [https://developer.download.nvidia.com/compute/DCGM/docs/nvidia-smi-367.38.pdf](https://developer.download.nvidia.com/compute/DCGM/docs/nvidia-smi-367.38.pdf)" width=80% height=80% >}}



## GPU Resource Usage

`gpustat`  will report GPU utilization and memory usage.
```bash
> module load gpustat
> gpustat
```

{{< figure src=/notes/deep-learning-hpc/img/gpustat.png caption="Source: https://github.com/wookayin/gpustat" width=90% height=90% >}}


### PyTorch

* Correct GPU memory usage will be reported by the previous tools.

### TensorFlow/Keras

* By default, TensorFlow automatically allocates ALL of the GPU memory so the previous tools will show that all (or almost all) of the GPU memory is being used.
* To track the amount of GPU memory actually used, you can add these lines to your python script:
```python
import os
os.environ['TF_FORCE_GPU_ALLOW_GROWTH'] = 'true'
```

Visit the [Tensorflow website](https://www.tensorflow.org/guide/gpu#limiting_gpu_memory_growth) for additional information.


## Check Your Knowledge

* Find the name of the GPU that you have access to.