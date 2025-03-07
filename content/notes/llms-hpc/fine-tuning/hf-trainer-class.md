---
title: Hugging Face Trainer Class
date: 2025-02-23-19:06:23Z
type: docs 
weight: 2800
menu: 
    llms-hpc:
      parent: Fine-Tuning
---

The Trainer class allows a user to train or fine-tune a model using a convenient function, rather than using native PyTorch.

When training, the Trainer will automatically use the GPU if one is present.

There are  __many__ options that can be set for the TrainingArguments (number of epochs, learning rate, save strategy, etc).
  * [More TrainingArguments](https://huggingface.co/docs/transformers/main_classes/trainer#transformers.TrainingArguments)

The Trainer will not automatically evaluate the LLM, so we will pass it an evaluation metric.


