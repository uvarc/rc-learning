---
title: Advantages of Accelerate
date: 2025-07-08T20:40:54Z
type: docs 
weight: 1500
menu: 
    multigpu-inference:
        parent: Accelerate
---

If you need to prototype or are expecting to run models on “fewer” GPUs, Accelerate is the preferred framework.

It is mostly plug-and-play with existing code, with options already built-in to transformers and classes that easily wrap with torch.

You can use it both for inference and for training.

There is some fine-grained control of GPU and CPU usage, allowing a degree of customization for improved performance depending on your use case.



