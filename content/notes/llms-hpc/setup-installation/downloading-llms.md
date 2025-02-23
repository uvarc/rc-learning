---
title: Downloading LLMs on UVA HPC
date: 2025-02-23-19:06:23Z
type: docs 
weight: 750
menu: 
    llms-hpc:
      parent: Setup/Installation
---


* When you use a transformers LLM for inference, it is downloaded to your home account
  * ~/.cache/huggingface/hub
* Datasets (from the datasets package) are also downloaded to your home account
  * ~/.cache/huggingface/datasets
* Make sure you have enough storage in your home account!
* As of now, each user has 50GB of home storage.  But after the October 15th maintenance, each user will have 200GB of home storage.

