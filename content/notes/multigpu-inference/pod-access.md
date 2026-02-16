---
title: POD Access 
date: 2025-07-08T20:40:54Z
type: docs 
weight: 550
menu: 
    multigpu-inference:
        parent: UVA HPC
---

__Open OnDemand (OOD)__

Choose “GPU” as the Rivanna/Afton Partition.

Choose “NVIDIA A100” as GPU type and fill in number of GPUs.

Select “Yes” for Show Additional Options and type `--constraint=gpupod` in the SLURM Option textbox.

__SLURM__

```bash
#SBATCH -p gpu
#SBATCH --gres=gpu:a100:n   
#SBATCH -C gpupod
```
>n: requested # of GPUs/node

Before running a multi-node job, please make sure the job can scale well to 8 GPUs on a single node.

Multi-node jobs on the POD should request all GPUs on the nodes, i.e. `--gres=gpu:a100:8`.


More info: https://www.rc.virginia.edu/userinfo/hpc/basepod/#accessing-the-pod

### GPU Limit on UVA HPC 

The maximum number of GPUs you can request for a UVA HPC job is 32.

The maximum number of nodes is 4.

More info: https://www.rc.virginia.edu/userinfo/hpc/#job-queues



