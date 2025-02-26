---
title: Selecting a GPU for Inference
date: 2025-02-23-19:06:23Z
type: docs 
weight: 2400
menu: 
    llms-hpc:
      parent: Inference
---


* Select a GPU based on how much GPU memory you will need.
* The GPU memory will contain:
  * the LLM (i.e., the model weights),
  * input and output data, and
  * extra variables for the forward pass (about 20% of the LLM size).
* LLM Memory (B): (# parameters) * (# bytes/parameter)
  * # bytes/parameter depends on the model’s precision, e.g., fp32 is 4 bytes/parameter
* __GPU Memory Estimate for Inference (B):__ 1.2 * (LLM Memory in B)
* __NOTE:__ I have found this formula to underestimate UVA GPU memory.  It is most likely a ballpark estimate, but I recommend tracking GPU memory using the GPU Dashboard to make a more informed GPU selection.

---

Note: B is for bytes.

