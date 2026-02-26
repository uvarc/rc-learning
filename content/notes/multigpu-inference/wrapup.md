---
title: Wrap Up
date: 2025-07-08T20:40:54Z
type: docs 
weight: 2050
menu: 
    multigpu-inference:
---

**Recap**

Modern LLMs cannot fit into a single GPU and thus multi-GPU jobs are needed for LLM inference.

UVA HPC provides a variety of GPUs as well as the NVIDIA BasePOD (H200s were recently added).

Plan how many and which GPUs you will need before running your LLM inference job.

Use a well-established framework to implement model parallelism.
