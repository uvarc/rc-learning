---
title: Install transformers and datasets packages
date: 2025-02-23-19:06:23Z
type: docs 
weight: 700
menu: 
    llms-hpc:
        parent: Setup/Installation
---

Using the PyTorch container:

<small>```module load apptainer pytorch/2.4.0```</small>

<small>```apptainer exec $CONTAINERDIR/pytorch-2.4.0.sif pip install transformers datasets```</small>

* These packages are provided by Hugging Face (more details on Hugging Face in a bit).
* For the fine-tuning example we will do later today, we will also need to install the accelerate and evaluate packages.

<small>```apptainer exec $CONTAINERDIR/pytorch-2.4.0.sif pip install accelerate evaluate```</smallç>

