---
title: GPU Memory Requirements
date: 2025-07-08-20:40:54Z
type: docs 
weight: 250
menu: 
    multigpu-inference:
        parent: Introduction
---

Below are the GPU memory requirements for Google's Gemma 3 models for four different context windows. 

{{< table >}}

| __Model__ | __4K Tokens__ | __8K Tokens__ | __32K Tokens__ | __128K Tokens__ |
| :-: | :-: | :-: | :-: | :-: |
| 4B @ FP16 | __11 GB__ | __12.4 GB__ | __20.8 GB__ | __54.4 GB__ |
| 12B @ FP16 | __31.9 GB__ | __35 GB__ | __53.6 GB__ | __128 GB__ |
| 27B @ FP16 | __70.3 GB__ | __75.8 GB__ | __108.8 GB__ | __241 GB__ |
| 4B @ INT4 | __2.8 GB__ | __3.2 GB__ | __5.3 GB__ | __14 GB__ |
| 12B @ INT4 | __8 GB__ | __8.8 GB__ | __13.6 GB__ | __32.8 GB__ |
| 27B @ INT4 | __17.6 GB__ | __19 GB__ | __27.4 GB__ | __61 GB__ |
{{< /table >}}

>Models are given in parameter counts (billions) at specific precisions (i.e., floating point 16 or integer 4 quantization) and variable context windows.

{{< figure src=/notes/multigpu-inference/img/Multi_GPU_LLM_Inference_5.png >}}

