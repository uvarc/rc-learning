---
title: Resources and Partitions
date: 2023-12-11-14:11:14Z
type: docs 
weight: 200
menu: 
    slurm-from-cli:
---

An HPC **job** is a description of the resources required, any preparatory steps such as loading [modules](https://www.rc.virginia.edu/userinfo/rivanna/software/modules/) or otherwise setting up an environment, and the commands to run the software, along with any postprocessing that may be appropriate.

The job is specified through a special form of script often called a _batch script_.  Usually it is written in `bash`.

Resources include the quantity of time requested, the amount of memory, the number of cores per node, and if appropriate the number of nodes or the number and/or architecture of GPU.

In the abstract, a _queue_ is a sequence of jobs to be prioritized and handled. In a cluster, a queue, which Slurm calls a **partition**, is implemented with a group of compute nodes that provide a particular set of resources.

Slurm is a software package that manages the resources of the cluster and balances the demands of many competing job requests.  It consists of a _workload manager_, often called a scheduler, and a _slurmd_ "daemon" which runs on each node and handles the execution and monitoring of the jobs.

