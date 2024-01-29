---
title: The Slurm Resource Manager
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1
menu: 
    slurm-from-cli:
---

{{< figure src="/notes/slurm-from-cli/img/slurm_logo.png" width=30% >}}


[Slurm](https://slurm.schedmd.com/) is a  __resource manager__ (RM), also known as a  _queueing system_.

Resource managers are used to submit _jobs_ on a computing cluster to compute nodes from an access point generally called a  _frontend_.

Frontends are intended for editing, compiling, and very short test runs.  Production jobs go to the compute nodes through the queueing system.
