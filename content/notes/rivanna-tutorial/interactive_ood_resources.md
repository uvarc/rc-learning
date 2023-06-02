---
title: Choosing Resource Requests
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 670

menu:
  rivanna-tutorial:
    parent: Interactive Apps with Open OnDemand
---

| Field | Description |
| :-: | :-: |
| Number of cores | Used in parallel processing.  Your code must be modified to take advantage of using multiple cores. |
| Memory Request in GB | When dealing with Big Data, you will need to increase the amount of memory.  A good rule of thumb is to request 2 to 3 times the size of data that you are reading in or generating. |
| Work Directory | Allows you to change the working directory of a Jupyter Notebook to your /scratch folder. |
| Optional: Slurm Option | Allows you to provide advanced features, like requesting specific nodes or providing a reservation |
| Optional Group | Only needed if you are in more than 16 Rivanna groups.  You may need to force Rivanna to see your allocation. |
| Optional: GPU type for GPU partition &  Optional: Number of GPUs | Only needed in you are running on a GPU node.  The “default” for GPU type will put you on the first available GPU node. For now, the number of GPUS should be 1. |

Some fields on the Web Forms are blank, while others are set to default values.

The most important request will usually be the Memory Request.

