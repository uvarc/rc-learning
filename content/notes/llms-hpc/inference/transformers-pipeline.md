---
title: Transformers Pipeline
date: 2025-02-23-19:06:23Z
type: docs 
weight: 1850
menu: 
    llms-hpc:
      parent: Inference
---


* The transformers pipeline consists of a tokenizer, model, and post processing for getting model output.
* Pros
  * Easy to use.
  * Efficiently manages data batching and gpu memory for you – good for HPC!
* Cons
  * Harder to debug when something goes wrong.
* Recommendation:
  * Use pipeline first.
  * If you get errors, you may have to use the model directly to diagnose the problem.


Source and more information: [https://huggingface.co/docs/transformers/pipeline_tutorial](https://huggingface.co/docs/transformers/pipeline_tutorial)

---

Pipeline “Hides” details from the programmer – good and bad

Tokenizer runs on CPU

Model runs on GPU

