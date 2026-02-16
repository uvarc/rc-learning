---
title: Terminology
date: 2025-11-12T03:53:56Z
type: docs 
weight: 300
menu: 
    rio-intro:
        parent: HPC at UVA
---

**Node**

Nodes are a type of computer called a server. They generally have more power than a typical computer. 

They may have special hardware like graphical processing units (GPUs). 

There are two types of Nodes:
1.  _Login node_ - a server used for logging in and submitting jobs.
2.  _Compute node_ - a server that carries out the computational work.


**Core**
   
A core is an individual processor on a computer. 

**Memory**

In HPC, memory refers to the random-access memory on a node. 

**Storage**

In HPC, storage refers to disk storage visible from a node.


{{< figure src=/notes/rio-intro/img/riodiagram.png caption="Overview of the Rio HPC user access workflow, from PI request to job execution within the cluster environment." alt="A flowchart showing the process for accessing the Rio high-security HPC system. It begins with a PI submitting a VM request, followed by Research Computing provisioning the VM and user accounts. Users then connect to the cluster environment, which includes modules for software, partitions for hardware, and Slurm as the job scheduler." width=70% height=70% >}}
