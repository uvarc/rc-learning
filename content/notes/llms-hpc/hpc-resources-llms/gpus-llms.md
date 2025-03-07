---
title: GPUs for LLMs
date: "2025-02-23T00:00:00"
type: docs 
weight: 900
menu: 
    llms-hpc:
      parent: HPC Resources for LLMs
---


Because LLMs involve a huge number of computations, we need a form of parallelization to speed up the process.
For example, the free version of ChatGPT (based on GPT-3.5) has 175 billion parameters, while the paid version (based on GPT-4) has over 1 trillion parameters.

GPUs (graphics processing units) provide the needed parallelization and speed up.
All the major deep learning Python libraries (Tensorflow, PyTorch, Keras,…) support the use of GPUs and allow users to distribute their code over multiple GPUs.

New GPUs have been developed and optimized specifically for deep learning.

