---
title: Optimizing LLMs with Fine-Tuning
date: 2025-02-23-19:06:23Z
type: docs 
weight: 2700
menu: 
    llms-hpc:
        parent: Fine-Tuning
---

{{< figure src=/notes/llms-hpc/img/LLMS_on_HPC_11.png width=65% height=65% >}}


Fine-tuning builds on a pre-trained language model (LLM) by using a smaller, labeled dataset to specialize and improve its performance for a specific task. Pre-training requires a large dataset and is computationally demanding, but fine-tuning is much less resource-intensive and allows models to adapt to domain-specific needs efficiently.


Example:

[distilbert](https://huggingface.co/distilbert/distilbert-base-uncased)[/](https://huggingface.co/distilbert/distilbert-base-uncased)[distilbert](https://huggingface.co/distilbert/distilbert-base-uncased)[-base-uncased](https://huggingface.co/distilbert/distilbert-base-uncased) was pre-trained on [BookCorpus](https://huggingface.co/datasets/bookcorpus/bookcorpus) and [English Wikipedia](https://huggingface.co/datasets/legacy-datasets/wikipedia), ~25GB of data

[distilbert](https://huggingface.co/distilbert/distilbert-base-uncased-finetuned-sst-2-english)[/distilbert-base-uncased-finetuned-sst-2-english ](https://huggingface.co/distilbert/distilbert-base-uncased-finetuned-sst-2-english)was fine-tuned on [Stanford Sentiment Treebank (sst2)](https://huggingface.co/datasets/stanfordnlp/sst2), ~5MB of data




