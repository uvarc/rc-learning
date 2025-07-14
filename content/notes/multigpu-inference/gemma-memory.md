---
title: Gemma 3 Memory Requirements
date: 2025-07-08-20:40:54Z
type: docs 
weight: 950
menu: 
    multigpu-inference:
        parent: UVA HPC
---
Below are the GPU Memory Requirements for Google’s Gemma 3 models across four context windows. 

{{< table >}}
| __Model__ | __4K Tokens__ | __8K Tokens__ | __32K Tokens__ | __128K Tokens__ |
| :-: | :-: | :-: | :-: | :-: |
| 4B @ FP16 | 9.6 + 1.4 =  __11 GB__ | 9.6 + 2.8 =  __12.4 GB__ | 9.6 + 11.2 =  __20.8 GB__ | 9.6 + 44.8 =  __54.4 GB__ |
| 4B @ INT4 | 2.4 + 0.4 =  __2.8 GB__ | 2.4 + 0.8 =  __3.2 GB__ | 2.4 + 2.9 =  __5.3 GB__ | 2.4 + 11.6 =  __14 GB__ |
| 12B @ FP16 | 28.8 + 3.1 =  __31.9 GB__ | 28.8 + 6.2 =  __35 GB__ | 28.8 + 24.8 =  __53.6 GB__ | 28.8 + 99.2 =  __128 GB__ |
| 12B @ INT4 | 7.2 + 0.8 =  __8 GB__ | 7.2 + 1.6 =  __8.8 GB__ | 7.2 + 6.4 =  __13.6 GB__ | 7.2 + 25.6 =  __32.8 GB__ |
| 27B @ FP16 | 64.8 + 5.5 =  __70.3 GB__ | 64.8 + 11 =  __75.8 GB__ | 64.8 + 44 =  __108.8 GB__ | 64.8 + 176 =  __241 GB__ |
| 27B @ INT4 | 16.2 + 1.4 =  __17.6 GB__ | 16.2 + 2.8 =  __19 GB__ | 16.2 + 11.2 =  __27.4 GB__ | 16.2 + 44.8 =  __61 GB__ |
{{< /table >}}

Models are given in parameter counts (billions) at specific precisions (i.e., floating point 16 or integer 4 quantization) and variable context windows:

* **4B**: 34 Layers, 2560 Embedding Dim
* **12B**: 48 Layers, 3840 Embedding Dim
* **27B**: 62 Layers, 5376 Embedding Dim


Each calculation is a breakdown of M (Model Size) + N (QKV Cache Size) from [Calculating GPU Memory Requirements](notes/multigpu-inference/calculating-gpu.md).

