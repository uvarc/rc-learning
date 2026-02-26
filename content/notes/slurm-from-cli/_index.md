---
title: The Slurm Resource Manager
date: 2023-12-11T00:00:00Z
type: docs 
weight: 1
menu: 
    slurm-from-cli:
        name: The Slurm Resource Manager
---

{{< figure src="/notes/slurm-from-cli/img/slurm_logo.png" width=30% alt="Slurm resource manager logo" >}}


[Slurm](https://slurm.schedmd.com/ "The official Slurm website") is a  __resource manager__ (RM), also known as a  _queueing system_.

Resource managers are used to submit _jobs_ on a computing cluster to compute nodes from an access point generally called a  login node.

Login nodes are intended for editing, compiling, and very short test runs.  Production jobs go to compute nodes through the queueing system.
