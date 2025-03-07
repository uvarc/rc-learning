---
title: When to Use CPU vs. GPU
date: "2025-02-23T00:00:00"
type: docs 
weight: 950
menu: 
    llms-hpc:
        parent: HPC Resources for LLMs
---

{{< table >}}
| Task | CPU or GPU |
| --- | --- |
| Tokenization | CPU |
| LLM Training/Fine-tuning | GPU |
| LLM Inference | Either, but GPU recommended |
{{< /table >}}

**When you request memory for HPC, that is CPU memory.**

If you request a GPU, you will receive all of that GPU’s memory.

