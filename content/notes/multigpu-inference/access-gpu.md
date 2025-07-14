---
title: GPU access on UVA HPC
date: 2025-07-08-20:40:54Z
type: docs 
weight: 450
menu: 
    multigpu-inference:
      parent: UVA HPC 
---

Note: When you request memory for HPC, that is CPU memory.

__Open OnDemand (OOD)__

Choose “GPU” or “Interactive” as the Rivanna/Afton Partition.

Optional: choose GPU type and number of GPUs.

You cannot use H200 GPUs directly through OOD.
  
__SLURM__

Specify GPU partition: `#SBATCH -p gpu`

Request n GPUs: `#SBATCH --gres=gpu:<optional_gpu_name>:n`
  * Examples: 
    * Request 1 GPU: `#SBATCH --gres=gpu:1`
    * Request 4 A100s: `#SBATCH --gres=gpu:a100:4`

To request 80GB A100,  __additionally__  use: `#SBATCH --constraint=a100_80gb`

Currently, only one person can be using a GPU at a time. If you request a GPU, you will receive all of the GPU memory. 

More info: https://www.rc.virginia.edu/userinfo/hpc/slurm/#gpu-intensive-computation




