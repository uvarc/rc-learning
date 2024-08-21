---
title: CPU Memory and Cores
date: 2024-06-28-01:51:43Z
type: docs 
weight: 500
toc: true
menu: 
    deep-learning-hpc:
      parent: Resource Allocation and Helpful Tools
---

## Request CPU Memory

* __Standard Partition (no GPU):__ you will get 9GB RAM per core
* __Interactive Partition:__ you will get 6GB RAM per core
* __GPU Partition:__ you can specify how much RAM you want

You should have enough RAM to comfortably work with your GPU. In other words, request at least as much RAM as the GPU you select.
* If you select multiple GPUs, request as much RAM as the GPU you selected with the largest memory.
* If you are using a large dataset and/or want to do extensive preprocessing, more RAM is probably helpful.
    * How much more?  Depends!  You can experiment and check your memory efficiency.

Visit this [Deep Learning Hardware Guide](https://timdettmers.com/2018/12/16/deep-learning-hardware-guide/) for more information.


## Request CPU Cores

It depends how many CPU Cores to request!  Generally, make your best guess to start.  Then check the  CPU and GPU efficiency of your script and adjust from there.
* __Are you are doing any data preprocessing on the CPU prior to training the network on the GPU?__
  * Is the preprocessing code serial or parallel?
  * NOTE: Even if your code is written as a serial program, NumPy automatically uses multiple cores for linear algebra operations!
* __Are you using a single core or multiple cores for the data loading from the CPU to the GPU for the training process?__
  * Use enough CPU cores to keep the GPU busy


### PyTorch

* PyTorch's DataLoader has a `num_workers` parameter, which is the number of CPU cores to use for the data loading.
* The default is `num_workers=1`, but this may not load data fast enough to keep the GPU busy.
* Try increasing `num_workers` to improve GPU efficiency and speed up DL code.


### Keras

* Keras will use multiple cores for data loading automatically
 