---
date : "2025-02-26T00:00:00"
title: "GPU Acceleration for Deep Learning"
toc: true
type: book
weight: 15

---


### **Why Use GPUs?**

Neural Networks can grow large and contain many million if not billions of parameters. ChatGPT free version (based on GPT-3.5) has 175 billion parameters, and ChatGPT paid version (based on GPT-4) has over 1 trillion parameters. Because training often involves millions of operations, we need a form of parallelization to speed up the process. GPUs are optimized for **parallel computations**, making them ideal for training deep learning models. They accelerate matrix operations, which are the core of neural network computations. Training large models on CPUs can take **days or weeks**, whereas GPUs can **reduce training time** significantly. All the major deep learning Python libraries (Tensorflow, PyTorch, Keras, Caffe,…) support the use of GPUs and allow users to distribute their code over multiple GPUs. New processors have been developed and optimized specifically for deep learning, like Google's Tensor Processing Unit.


#### **Key Features**
- **Thousands of cores** designed for high throughput optimal for parallel computations
- **High memory bandwidth** for efficient data transfer
- Optimized for **tensor operations** (e.g., matrix multiplications)

If you’re working with a small model or smaller dataset, you may find using a GPU slows down your work.This is mainly due to the overhead cost of transfering data back and forward to the CPU. To find out if your task could benefit from using GPUs, it’s important to benchmark and profile your code. Learn more about Benchmarking and Profiling here: https://learning.rc.virginia.edu/notes/benchmark-parallel-programs/ https://learning.rc.virginia.edu/courses/python-high-performance/

---

###  **PyTorch and GPUs**
#### **What is CUDA?**
- **CUDA (Compute Unified Device Architecture)** is NVIDIA’s parallel computing platform and API. It allows PyTorch to leverage **GPU acceleration** efficiently.

- **cuDNN (CUDA Deep Neural Network Library)** is a GPU-accelerated library for deep learning primitives.It provides optimized implementations for convolutions, activation functions, and other operations.

These tools come installed with the GPU version of PyTorch. To use PyTorch with a CUDA compatible device (Nvidia GPUs), ensure that you install the correct version from the pytorch website. The good news; You don't need to know how to use them to take advantage of GPU acceleration!

#### **Using GPUs in PyTorch**
PyTorch makes it easy to move computations between CPU and GPU.
We need to explicitly tell PyTorch to use the CPU or the GPU. If a GPU is available, it will **not** automatically use it.
``` python
if torch.cuda.is_available():
    device = "cuda"
else:
    device = "cpu"

# or simply
device = "cuda" if torch.cuda.is_available() else "cpu"
```
When working with GPUs, inputs must be sent to the device explicitly. This includes your data and the NN model.
```python
# Move model inputs to device
x_train, y_train = x_train.to(device) , y_train.to(device)

# Move model to device
model = MyNeuralNetwork()
model.to(device)
```
---

### **Writing SLURM Scripts for PyTorch Jobs**
Our High-Performance Computing (HPC) clusters use SLURM (Simple Linux Utility for Resource Management) to manage jobs. To run full training jobs submit a slurm script to the GPU partition. When using the GPU partition, your SLURM script must include the --gres=gpu option.

Example SLURM Script for PyTorch Training
Save the following script as train.slurm:
~~~sh
#!/bin/bash
#SBATCH -A mygroup
#SBATCH -p gpu         
#SBATCH --gres=gpu:1    
#SBATCH -c 1
#SBATCH -t 00:01:00
#SBATCH --job-name=pytorch_training
#SBATCH --output=logs/output_%j.log   # Save standard output
#SBATCH --error=logs/error_%j.log     # Save error output


module purge
module load apptainer pytorch/2.0.1 

# Optional: Activate virtual environment
source activate myenv

apptainer run --nv $CONTAINERDIR/pytorch-2.0.1.sif pytorch_script.py 
~~~
Then submit your job:
~~~sh
sbatch train.slurm
~~~
To show current jobs:
~~~sh
show job status squeue -u $computing_id
# or
sstat job_id
~~~
Alternatively you can use the [SLURM Script Generator](https://www.rc.virginia.edu/userinfo/hpc/slurm-script-generator/) to create your script.


For more information on SLURM visit: https://www.rc.virginia.edu/userinfo/hpc/software/pytorch/, https://www.rc.virginia.edu/userinfo/hpc/slurm/

For information on multi-GPU use: https://pytorch.org/tutorials/beginner/ddp_series_multigpu.html