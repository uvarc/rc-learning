---
title: GPUs for Deep Learning
date: 2024-06-28T01:51:43Z
type: docs 
weight: 300
toc: true
menu: 
    deep-learning-hpc:
      parent: Hardware Overview
---


Because the training process involves hundreds of thousands of computations, we need a form of parallelization to speed up the process.

  * For instance, ChatGPT's free version (based on GPT-3.5) uses a model with 175 billion parameters,
  * whereas ChatGPT's paid version (based on GPT-4) uses a model with over 1 trillion parameters
  * Although Neural Network calculations are very simple, there are a lot of them!

GPUs (graphics processing units) provide the needed parallelization and speed up.

## Deep Learning using GPUs
* All the major deep learning Python libraries (Tensorflow, PyTorch, Keras, etc.) support the use of GPUs and allow users to distribute their code over multiple GPUs.
* New GPUs have been developed and optimized specifically for deep learning.
* Scikit-learn does not support GPU processing.
* Deep learning acceleration is furthered with Tensor Cores in NVIDIA GPUs.
  * Tensor Cores accelerate large matrix operations by performing mixed-precision computing. It accelerates math and reduces the memory traffic and consumption.

### Neural Networks

* If you're  __not__  using a neural network as your machine learning model you may find that a GPU doesn't improve the computation time.
* If you are using a neural network but it is very small then a GPU will not be any faster than a CPU - in fact, it might even be slower.


## General GPU Workflow

The following is a high-level summary of the general GPU workflow, skipping memory allocations: 

* Create data on the CPU
* Send data from the CPU to the GPU (for DL this is done in batches)
* Compute result on the GPU
* Send the result back to the CPU

Depending on the Deep Learning framework you are using, some of these steps may be automatically done for you.
