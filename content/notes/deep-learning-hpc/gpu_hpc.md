---
title: GPUs on HPC
date: 2024-06-28T01:51:43Z
type: docs 
weight: 350
toc: true
menu: 
    deep-learning-hpc:
      parent: Hardware Overview
---

{{< table >}}
| GPU | Full Name | Year Launched | Memory | # of Tensor Cores |
| --- | --- | --- | --- | ---- |
| A100 | NVIDIA A100 | 2020 | 40GB or 80GB | 432 (3<sup>rd</sup> gen) |
| A6000 | NVIDIA RTX A6000 | 2020 | 48GB | 336 (3<sup>rd</sup> gen) |
| A40 | NVIDIA A40 | 2020 | 48GB | 336 (3<sup>rd</sup> gen) |
| RTX3090 | NVIDIA GeForce RTX 3090 | 2020 | 24GB | 328 (3<sup>rd</sup> gen) |
| RTX2080Ti | NVIDIA GeForce RTX 2080 Ti | 2018 | 11GB | 544 (2<sup>nd</sup> gen) |
| V100 | NVIDIA V100 | 2018 | 32GB | 640 (1<sup>st</sup> gen) |
{{< /table >}}

## UVA-NVIDIA DGX BasePOD

* 10 DGX A100 nodes
  * 8 NVIDIA A100 GPUs.
  * 80 GB GPU memory options.
  * Dual AMD EPYC:tm:; nodes: Series 7742 CPUs, 128 total cores, 2.25 GHz (base), 3.4 GHz (max boost).
  * 2 TB of system memory.
  * Two 1.92 TB M.2 NVMe drives for DGX OS, eight 3.84 TB U.2 NVMe drives forstorage/cache.
* Advanced Features:
  * NVLink for fast multi-GPU communication
  * GPUDirect RDMA Peer Memory for fast multi-node multi-GPU communication
  * GPUDirect Storage with 200 TB IBM ESS3200 (NVMe) SpectrumScale storage array
* Ideal Scenarios:
  * Job needs multiple GPUs on a single node or multi node
  * Job (single or multi-GPU) is I/O intensive
  * Job (single or multi-GPU) requires more than 40GB of GPU memory

Note: The POD is good if you need multiple GPUs and very fast computation.


## GPU access on UVA HPC

**When you request memory for UVA HPC, that is CPU memory.**

**If you request a GPU, you will receive all of the GPU memory.**

* Choose "GPU" or "Interactive" as the HPC Partition in OOD
* Optionally, choose GPU type and number of GPUs
* POD nodes are contained in the gpu partition with a specific Slurm constraint.
* Slurm script:
```bash
#SBATCH -p gpu
#SBATCH --gres=gpu:a100:X	# X number of GPUs
#SBATCH -C gpupod
```
* Open OnDemand
```nohighlight
--constraint=gpupod
```

Note: Only one person can be using a GPU at a time.