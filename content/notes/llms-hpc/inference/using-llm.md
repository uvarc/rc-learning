---
title: Using an LLM
date: 2025-02-23-19:06:23Z
type: docs 
weight: 1800
menu: 
    llms-hpc:
      parent: Inference
---


* LLMs can be used as-is (i.e., out-of-the-box) or after fine-tuning.
* Hugging Face model cards will generally provide code for how to get started.
  * Code may be PyTorch or TensorFlow, “raw” (using the model directly), or pipeline code (using the pipeline from transformers library).
  * Ex 1: [https://huggingface.co/distilbert/distilbert-base-uncased-finetuned-sst-2-english](https://huggingface.co/distilbert/distilbert-base-uncased-finetuned-sst-2-english)
    * provides “raw” PyTorch code
  * Ex 2: [https://huggingface.co/facebook/bart-large-cnn](https://huggingface.co/facebook/bart-large-cnn)
    * provides pipeline code
* Code for at least loading the model (directly and using the pipeline) is provided by clicking the “Use this model” button on Hugging Face.
  * You may have to dig through the links to find the code you need.

