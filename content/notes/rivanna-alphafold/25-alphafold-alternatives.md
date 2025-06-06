---
title: AlphaFold Alternatives 
date: 2025-05-20-00:23:54Z
type: docs 
weight: 2400
menu: 
    rivanna-alphafold:
---

**Openfold**
Openfold is a PyTorch reproduction of AlphaFold. It is trainable, and has a `long sequences` argument that allows you to run longer sequences that AlphaFold cannot fit into memory on single GPU. It does not support multi-GPU for model inference--only for training. 

**Fastfold**
Fastfold is another trainable alternative that supports multi-GPU utilization for training and model inference. 

**Parafold**
Parafold can be used to accelerate AlphaFold's prediction of multiple sequences by dividing CPU (MSA and template searching) from GPU (prediction model).



