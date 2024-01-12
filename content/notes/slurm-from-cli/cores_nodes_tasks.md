---
title: Cores, Nodes, and Tasks
date: 2023-12-11-14:11:14Z
type: docs 
weight: 300
menu: 
    slurm-from-cli:
---

## Hardware

The Slurm model is a cluster consisting of a number of **nodes**.  Each node is a separate server.  These servers are similar to an ordinary desktop computer, but are more reliable and usually provide more memory and cores that an ordinary desktop.

A **core** is a computing unit. It is part of a **cpu**.  

{{< alert >}}
Slurm began when cpus had only one core each. Beginning around 2005, cpus began to be divided into multiple cores.  But Slurm still refers to cores as "cpus."
{{< /alert >}}

**Memory** refers to _random-access memory_.  It is not the same thing as storage.  If a process reports running out of memory, it means RAM memory. Running out of disk space will result in a different error.

For more details about the structure of a computational cluster, see our [introduction](/notes/rivanna-command-line/overview_of_rivanna/overview_terminology).

## Processes and Tasks

A **process** can be envisioned an instance of an executable that is running on a particular computer.  Most executables run only a single process.  Some executables run _threads_ within the _root_ process.

Slurm refers to the root process as a **task**. By default, each task is assigned to one core.


