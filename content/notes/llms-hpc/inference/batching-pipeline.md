---
title: Pipeline Batching with GPU
date: 2025-02-23-19:06:23Z
type: docs 
weight: 2150
menu: 
    llms-hpc:
      parent: Inference
---


* Data (sequences) are passed in batches to the GPU, instead of one at a time
* This allows the GPU to stay busy computing without waiting on more data to be passed from the CPU
* Batching can be used if the pipeline is passed a list of data or a dataset from the datasets package
* Batching may or may not speed up your code!  You will need to test it.
* The default batch_size for a pipeline is 1.
* If a dataset from the datasets package is used, DataLoader is being called under the hood in the pipeline.
  * Use multiple CPU cores and set the num_workers parameter (default is 8).


Source and more information: [https://huggingface.co/docs/transformers/en/main_classes/pipelines#pipeline-batching](https://huggingface.co/docs/transformers/en/main_classes/pipelines#pipeline-batching)

---

Note: Do not use batching on cpu.

