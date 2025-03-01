---
title: General GPU workflow
date: 2025-02-23-19:06:23Z
type: docs 
weight: 1000
menu: 
    llms-hpc:
        parent: HPC Resources for LLMs
---


* Create data on the CPU.

* Send data from the CPU to the GPU (for DL this is done in batches).

* Compute result on the GPU.

* Send the result back to the CPU.


* Depending on the DL framework/LLM pipeline you are using, some of these steps may be automatically done for you.

