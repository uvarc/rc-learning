---
title: UVA BasePOD
date: 2025-07-08-20:40:54Z
type: docs 
weight: 500
menu: 
    multigpu-inference:
        parent: UVA HPC 
---

### UVA HPC – NVIDIA DGX BasePOD on Rivanna/Afton

UVA's DGX BasePOD is a shared high-performance GPU resource available on both the Rivanna and Afton clusters. It can be used to support large deep-learning models. 

The BasePOD includes __18 DGX A100 nodes__ with:

* 2TB of RAM memory per node

* 8 A100s per node

* 80 GB of GPU memory per GPU device

__Advanced Features__ (compared to regular GPU nodes):

* NVLink for fast multi-GPU communication

* GPUDirect RDMA Peer Memory for fast multi-node multi-GPU communication

* GPUDirect Storage with 200 TB IBM ESS3200 (NVMe) SpectrumScale storage array

__Ideal Scenarios__:

* Job needs multiple GPUs on a single node or even multiple nodes

* Job (single or multi-GPU) is I/O intensive

If you have ever used an A100 with 80 GB on our system, you were using a POD node!

More info: https://www.rc.virginia.edu/userinfo/hpc/basepod/

