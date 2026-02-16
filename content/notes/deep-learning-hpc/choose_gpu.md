---
title: Choose a GPU
date: 2024-06-28T01:51:43Z
type: docs 
weight: 850
toc: true
menu: 
    deep-learning-hpc:
---


## GPUs on HPC

{{< table >}}
| GPU | Full Name | Year Launched | Memory | # of Tensor Cores |
| --- | --- | --- | --- | --- |
| A100 | NVIDIA A100 | 2020 | 40GB or 80GB | 432 (3rd gen) |
| A6000 | NVIDIA RTX A6000 | 2020 | 48GB | 336 (3rd gen) |
| A40 | NVIDIA A40 | 2020 | 48GB | 336 (3rd gen) |
| RTX3090 | NVIDIA GeForce RTX 3090 | 2020 | 24GB | 328 (3rd gen) |
| RTX2080Ti | NVIDIA GeForce RTX 2080 Ti | 2018 | 11GB | 544 (2nd gen) |
| V100 | NVIDIA V100 | 2018 | 32GB | 640 (1st gen) |
{{< /table >}}


## Wait Time in the Queue

* You may not need to request an A100 GPU!
* Requesting an A100 may mean you wait in the queue for a much longer time than using another GPU,
* This could give you a slower overall time (wait time + execution time) than if you had used another GPU.

{{< figure src=/notes/deep-learning-hpc/img/queue_wait_graph.png caption="Photo Source: [https://researchcomputing.princeton.edu/support/knowledge-base/scaling-analysis](https://researchcomputing.princeton.edu/support/knowledge-base/scaling-analysis)" width=80% height=80% >}}


## Memory Required to Train a DL Model

Generally, you will choose a GPU based on how much GPU memory you need. But, it is a hard problem to determine how much GPU memory a DL model will need for training  __before__  training the model. 
* In addition to storing the DL model, training also requires additional storage space such as:
  * Optimizer states
  * Gradients
  * Data (how much is determined by the batch size)
* Training can also use automatic mixed precision which lowers the amount of memory needed

Visit [https://blog.eleuther.ai/transformer-math/](https://blog.eleuther.ai/transformer-math/) for more information on math related to computation and memory usage for transformers.


## General Advice

* If you are learning about DL and doing tutorials, the GPUs in the Interactive partition are probably fine.
* You can leave the GPU choice as default on the GPU partition and work on whichever GPU you get or choose a GPU with a smaller amount of memory first.
  * Train your model for one epoch and monitor the GPU memory usage.
  * Use this information to choose a GPU to do the complete training on.
* You can calculate the size of your DL model (the number of parameters) to compute the memory needed to store the model. See [here](https://wandb.ai/wandb_fc/tips/reports/How-To-Calculate-Number-of-Model-Parameters-for-PyTorch-and-TensorFlow-Models--VmlldzoyMDYyNzIx) for details.
* There is a tool on Hugging Face that can calculate memory needs for a transformers or timm model (using a batch size of 1): [https://huggingface.co/spaces/hf-accelerate/model-memory-usage](https://huggingface.co/spaces/hf-accelerate/model-memory-usage)
* Providing more information to users on how to choose a GPU for DL is currently being worked on.
* Information will be updated on our website as it becomes available.

 
