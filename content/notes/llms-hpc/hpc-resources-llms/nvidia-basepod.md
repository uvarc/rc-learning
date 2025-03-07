---
title: UVA HPC - NVIDIA DGX BasePOD
date: "2025-02-23T00:00:00"
type: docs 
weight: 1100
menu: 
    llms-hpc:
      parent: HPC Resources for LLMs
---


* 10 DGX A100 nodes
  * 8 NVIDIA A100 GPUs.
  * 80 GB GPU memory options.
  * Dual AMD EPYC™ 7742 CPUs, 128 total cores, 2.25 GHz (base), 3.4 GHz (max boost).
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

---

The POD is good if you need multiple GPUs and very fast computation.

