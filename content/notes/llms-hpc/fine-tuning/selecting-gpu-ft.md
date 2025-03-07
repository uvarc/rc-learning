---
title: Selecting a GPU for Fine-Tuning
date: "2025-02-23T00:00:00"
type: docs 
weight: 3000
menu: 
    llms-hpc:
      parent: Fine-Tuning
---

For fine-tuning, select a GPU based on how much GPU memory you will need.

But, it is a hard problem to determine how much GPU memory a LLM will need for training  __before__  training the model.

A training iteration requires a forward and backward pass of the model. In addition to storing the LLM, training also requires additional storage space such as:
  * Optimizer states
  * Gradients
  * Activations
  * Data (how much is determined by the batch size)

---

According to the [Hugging Face Model Memory Calculator](https://huggingface.co/spaces/hf-accelerate/model-memory-usage), for a batch size of 1,  $$ \text{GPU Memory Estimate for Fine-Tuning (B)} = 4 \times (\text{LLM Memory in B})$$ 
  * While this formula can help ballpark an estimate, I recommend tracking GPU memory using the GPU Dashboard to make a more informed GPU selection.

For a more specific formula, see [https://blog.eleuther.ai/transformer-math/](https://blog.eleuther.ai/transformer-math/) (Note: this blog requires some understanding of transformers).


Determining LLM memory for fine-tuning is an active area of research.  The paper [LLMem](https://arxiv.org/abs/2404.10933)[: Estimating GPU Memory Usage for Fine-Tuning Pre-Trained LLMs](https://arxiv.org/abs/2404.10933) by Kim, et al. (April 2024) presents a method for doing so within 3% of the actual memory required.

