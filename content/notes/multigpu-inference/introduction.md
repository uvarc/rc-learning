---
title: Introduction
date: 2025-07-08-20:40:54Z
type: docs 
weight: 100
menu: 
    multigpu-inference:
      name: Introduction
---

### Terminology


{{< table >}}
| Term               | Definition                                                                |
|--------------------|---------------------------------------------------------------------------|
| **VRAM**           | Video RAM (GPU Memory).                                                   |
| **Context Window** | The maximum number of tokens an LLM can keep “in-memory”.                |
| **Precision**      | The amount of storage used for an LLM parameter.*                         |
| **Quantization**   | A technique to lower the amount of storage used by LLM parameters.       |
| **Embedding Dimension** | The length of the embedding vector.                              |
| **QKV Cache**      | Query Key Value Cache is the short term memory of an LLM.                |
{{< /table >}}

<small> \* For example, fp32 is 4 bytes/parameter, and fp16 is 2 bytes/parameter. </small>

### Why use multi-GPU LLM inference?

LLMs are getting larger and larger, as are their context windows. More recent LLMs generally do not fit in a single GPU due to VRAM limits. This will be the focus case of the workshop. 

