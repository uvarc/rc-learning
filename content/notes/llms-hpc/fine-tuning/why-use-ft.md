---
title: Optimizing LLMs with Fine-Tuning
date: "2025-02-23T00:00:00"
type: docs 
weight: 2700
menu: 
    llms-hpc:
        parent: Fine-Tuning
---

{{< figure src=/notes/llms-hpc/img/LLMS_on_HPC_11.png alt="Comparison diagram showing pre-training on a large unlabeled dataset as computationally demanding and fine-tuning on a small labeled dataset as computationally inexpensive." width=65% height=65% >}}


Fine-tuning builds on a pre-trained language model (LLM) by using a smaller, labeled dataset to specialize and improve its performance for a specific task. Pre-training requires a large dataset and is computationally demanding, but fine-tuning is much less resource-intensive and allows models to adapt to domain-specific needs efficiently.


Example:

The model [distilbert-base-uncased](https://huggingface.co/distilbert/distilbert-base-uncased) was pre-trained on [BookCorpus](https://huggingface.co/datasets/bookcorpus/bookcorpus) and [English Wikipedia](https://huggingface.co/datasets/legacy-datasets/wikipedia), about 25 GB of data.

The model [distilbert-base-uncased-finetuned-sst-2-english](https://huggingface.co/distilbert/distilbert-base-uncased-finetuned-sst-2-english) was fine-tuned on the [Stanford Sentiment Treebank (SST-2)](https://huggingface.co/datasets/stanfordnlp/sst2), about 5 MB of data.




