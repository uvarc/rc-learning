---
title: Exercise 3c
date: 2025-02-23-19:06:23Z
type: docs 
weight: 2250
menu: 
    llms-hpc:
        parent: Inference
---

## Batch Size and GPU Memory

* As batch_size increases, so does GPU memory usage.
* If you get an OOM (out of memory) error while using the GPU, try decreasing the LLM batch size.

Source and more information: [https://huggingface.co/docs/transformers/en/main_classes/pipelines#pipeline-batching](https://huggingface.co/docs/transformers/en/main_classes/pipelines#pipeline-batching)


## Exercise 3c: Batch Size and Num Workers - Text Summarization

* Open the ex3c.ipynb file from the workshop folder.

* Run each cell of this notebook and complete the EXERCISES as you go.

* Watch the GPU memory using GPU Dashboard as you run the cells.





