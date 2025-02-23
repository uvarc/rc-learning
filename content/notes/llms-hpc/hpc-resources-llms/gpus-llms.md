---
title: GPUs for LLMs (or Deep Learning)
date: 2025-02-23-19:06:23Z
type: docs 
weight: 900
menu: 
    llms-hpc:
      parent: HPC Resources for LLMs
---


* Because LLMs involve a huge number of computations, we need a form of parallelization to speed up the process.
  * Ex: ChatGPT free version (based on GPT-3.5): 175 billion parameters,
  * ChatGPT paid version (based on GPT-4): over 1 trillion parameters
* GPUs (graphics processing units) provide the needed parallelization and speed up.
* All the major deep learning Python libraries (Tensorflow, PyTorch, Keras,…) support the use of GPUs and allow users to distribute their code over multiple GPUs.
* New GPUs have been developed and optimized specifically for deep learning.

