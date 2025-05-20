---
title: Other options
date: 2025-05-20-00:23:54Z
type: docs 
weight: 2400
menu: 
    rivanna-alphafold:
---



* Openfold
  * PyTorch reproduction of AlphaFold
  * Trainable
  * has “long sequences” argument that allows you to run longer sequences that Alphafold can’t fit into memory on a single GPU
  * Does not support multi-GPU for model inference, only for training
* Fastfold
  * Trainable
  * Supports multi-GPU utilization for training & model inference
* Parafold
  * Divides CPU (MSA & template searching) from GPU (prediction model) to accelerate prediction of multiple sequences



