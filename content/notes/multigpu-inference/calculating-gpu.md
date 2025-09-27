---
title: Calculating GPU Memory Requirements
date: 2025-07-08-20:40:54Z
type: docs 
weight: 900
menu: 
    multigpu-inference:
        parent: UVA HPC
---

You must store 2 things when running an LLM on GPUs:
1. Model parameters and activations
2. QKV Cache for context 

For calculating memory requirements for model parameters and activations, a good general rule of thumb is the formula where **P** is the parameter count (in billions) and **Q** is bytes per parameter. 

$$
M = 1.2 \times \frac{4P}{32 / Q}
$$

For calculating memory requirements for QKV Cache, if **C** is the context size, **L** is number of layers, and **E** is the embedding dimension, use this formula: 

$$
N = C \times 2 \left( L \times E \times \frac{4}{32 / Q} \right)
$$
