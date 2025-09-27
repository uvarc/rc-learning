---
title: Choosing a Hugging Face Model
date: "2025-02-23T00:00:00"
type: docs 
weight: 1550
menu: 
    llms-hpc:
      parent: Model Selection
---

## Planning to Choose a Hugging Face Model

Things to consider:
* What type of task will you do?  (text classification, question answering, etc.)
* What type of data will you work with?  Is the text data from a specific domain (financial, scientific, Tweets, etc.)?  What language is it in?

More specific text data will most likely need a fine-tuned model, otherwise a more general LLM may work better.
If you need to fine-tune a model, do you have the computational resources to do so?
  
Larger models (i.e., models with more parameters) will need more resources.

[Source and more information](https://medium.com/@harshapulletikurti/choosing-the-correct-llm-model-from-hugging-face-hub-183fc6198295)


## Choosing the Model

Appropriately use model filters (task, language, license, etc).

Select either a general LLM or a fine-tuned model.

Check number of downloads. While a more popular model isn’t always better, it is good to know what models other people find useful.

Check model size (how to examples on next slides).

Read model card (documentation), including
  * The datasets that the model was trained on and fine-tuned on (if applicable).
  * The model license (does this meet your needs?)
  * Any benchmarking results.


[Source and more information](https://medium.com/@harshapulletikurti/choosing-the-correct-llm-model-from-hugging-face-hub-183fc6198295)

