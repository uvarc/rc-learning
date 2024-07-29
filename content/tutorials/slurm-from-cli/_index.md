---
date: "2023-01-04"
title: "Using Slurm from a Terminal"
summary: "This tutorial is an introduction to submitting Slurm jobs through a terminal interface."
authors: [as,kah]
categories: ["HPC"]
tags: [hpc]
weight: 30
notes: slurm-from-cli
---

{{< figure src="/tutorials/slurm-from-cli/img/slurm_logo.png" width=30% >}}

[Slurm](https://slurm.schedmd.com/) is a  __resource manager__ (RM), also known as a  _queueing system_.

Resource managers are used to submit _jobs_ on a computing cluster to compute nodes from an access point generally called a  login node.

Login nodes are intended for editing, compiling, and very short test runs.  Production jobs go to compute nodes through the queueing system.
