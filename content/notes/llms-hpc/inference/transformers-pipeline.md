---
title: Transformers Pipeline
date: "2025-02-23T00:00:00"
type: docs 
weight: 1850
menu: 
    llms-hpc:
      parent: Inference
---

The transformers pipeline consists of a tokenizer, model, and post processing for getting model output.

Pros:
  * Easy to use.
  * Efficiently manages data batching and gpu memory for you – good for HPC!

Cons:
  * Harder to debug when something goes wrong.

Recommendation: Use the pipeline first.
If you get errors, you may have to use the model directly to diagnose the problem.

The pipeline "hides" details from the programmer, which can be good and bad. 

The tokenizer runs on CPU, and the model runs on GPU.

[Source and more information](https://huggingface.co/docs/transformers/pipeline_tutorial)


