---
title: Advantages of vLLM
date: 2025-07-08-20:40:54Z
type: docs 
weight: 1900
menu: 
    multigpu-inference:
        parent: vLLM
---

You should use vLLM if you expect to serve lots of requests to LLMs for inference or if you have particularly long generated sequences.

vLLM can cut memory usage in some applications by 60-80% compared to vanilla transformers, though your mileage may vary.

vLLM is compatible with most Hugging Face models.

vLLM is generally used for large-scale serving; benefits over Accelerate are likely to be less pronounced in small use-cases (less than 1 million calls).



