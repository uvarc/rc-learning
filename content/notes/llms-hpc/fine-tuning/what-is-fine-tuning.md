---
title: What is Fine-Tuning?
date: 2025-02-23-19:06:23Z
type: docs 
weight: 2600
menu: 
    llms-hpc:
        name: Fine-Tuning
---


{{< figure src=/notes/llms-hpc/img/LLMS_on_HPC_10.png >}}

* LLMs are pre-trained on huge amounts of text data to learn general language patterns
* LLMs can be fine-tuned on a much smaller amount of data to excel at a particular task (e.g., classification of financial text)


Note: LLM pre-training is generally unsupervised.

## Types of Fine-Tuning

* Fine-tuning can be a supervised or unsupervised process and involves:
  * Changing some of the LLM weights,
  * __Changing all of the LLM weights (full fine-tuning),__ or
  * Parameter Efficient Fine-Tuning (PEFT), i.e., keeping the LLM weights the same but updating a small number of additional parameters that will adjust the LLM weights (e.g., LoRA).
* The more weights you update, the more computational resources you need



