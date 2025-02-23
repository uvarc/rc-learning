---
title: Terminology
date: 2025-02-23-19:06:23Z
type: docs 
weight: 300
menu: 
    llms-hpc:
        parent: Overview of LLMs
---


* LLMs can have billions of  __parameters__  (unknown quantities in the model)
* An LLM is  __trained __ (model parameters are determined) using a very large amount of text data
* A  __pre-trained__  LLM has already been trained.  This process allows the model to “learn” the language.
* A  __fine-tuned__  LLM is a pre-trained LLM that then is further trained on additional data for a specific task.  Model parameters are updated in the fine-tuning process.
* __Inference__  is the process in which a trained (or fine-tuned) LLM makes a prediction for a given input

* The input to an LLM is a  __sequence__  of  __tokens __ (words, characters, subwords, etc.)
* The  __batch size__ for an LLM is the number of sequences passed to the model at once
* Generally, raw text is passed through a  __tokenizer__  which processes it into tokens and sequences and then numerical data which can be sent to the LLM.

{{< figure src=/notes/llms-hpc/img/LLMS_on_HPC_2.png >}}