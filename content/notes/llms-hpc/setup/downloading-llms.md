---
title: Downloading LLMs on UVA HPC
date: 2025-02-23-19:06:23Z
type: docs 
weight: 750
menu: 
    llms-hpc:
      parent: Setup
---

When you use a transformers LLM for inference, it is downloaded to your home account.

``` ~/.cache/huggingface/hub```

Datasets (from the datasets package) are also downloaded to your home account.

``` ~/.cache/huggingface/datasets```

Make sure you have enough storage in your home account!

Currently, each user has access to 200GB of home storage.  

