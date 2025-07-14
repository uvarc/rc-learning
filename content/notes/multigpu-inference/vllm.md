---
title: vLLM
date: 2025-07-08-20:40:54Z
type: docs 
weight: 1800
menu: 
    multigpu-inference:
        name: vLLM
---

vLLM (virtual large language model) is an open-source library from UC Berkley. 

A key innovation for inference is PagedAttention, which is an efficient virtual memory/page method for storing the KV cache, which in longer contexts can end up committing more memory than the models themselves.

PagedAttention also allows continuous batching, which is a method for handling multiple requests to LLMs that enables less idle time.

Like DeepSpeed, it also supports optimized CUDA kernels for lower latency inference.

It is built on top of Megatron and can interface with DeepSpeed.



