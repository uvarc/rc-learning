---
title: General GPU workflow
date: "2025-02-23T00:00:00"
type: docs 
weight: 1000
menu: 
    llms-hpc:
        parent: HPC Resources for LLMs
---


1. Create data on the CPU.

2. Send data from the CPU to the GPU (for DL this is done in batches).

3. Compute result on the GPU.

4. Send the result back to the CPU.

Depending on the DL framework/LLM pipeline you are using, some of these steps may be automatically done for you.

