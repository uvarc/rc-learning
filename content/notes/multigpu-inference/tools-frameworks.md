---
title: Key Tools and Frameworks
date: 2025-07-08-20:40:54Z
type: docs 
weight: 1300
menu: 
    multigpu-inference:
        parent: Multi-GPU Strategies
---


Hugging Face  __Accelerate__ is an easy interface with transformers and PyTorch.

__DeepSpeed__ is good for large scale (thousand+ GPUs) well-optimized training and inference.

__vLLM__ focuses on high-speed, efficient LLM inference.

__Megatron__ is a low-level, open source API for custom development frameworks.

RC's resources are best used for Accelerate, unless there are specific inference/training-centric memory management or speed-ups required from DeepSpeed or vLLM. Accelerate will set up model parallelism for you. Code examples will be provided. 


More information: https://huggingface.co/docs/accelerate/en/concept_guides/big_model_inference
