---
title: Multicore and Multinode Jobs
date: 2023-12-11-14:11:14Z
type: docs 
weight: 2000
menu: 
    slurm-from-cli:
---

One of the advantages of using a high-performance cluster is the ability to use many cores and/or nodes at once.  This is called _parallelism_.  There are three main types of parallelism.

You should understand whether your program can make use of more than one core or node before you request multiple cores and/or nodes. Special programming is required to enable these capabilities.  Asking for multiple cores or nodes that your program cannot use will result in idle cores and wasted SUs, since you are charged for each core-hour. The [`seff`](/notes/slurm-from-cli/resource_utilization) command can help with this.

## High Throughput Serial Parallelism

High throughput parallelism is when many identical jobs are run at once, each on a single core.  Examples can include Monte-Carlo methods, parameter searches, image processing on many related images, some areas of bioinformatics, and many others.  For most cases of this type of parallelism, the best Slurm option is a [job array](/notes/slurm-from-cli/job_arrays).

When planning a high-throughput project, it is important to keep in mind that if the individual jobs are very short, less than roughly 15-30 minutes each, it is very inefficient to run each one separately, whether you do this manually or through an array.  In this case you should group your jobs and run multiple instances within the same job script.  Please [contact us](https://www.rc.virginia.edu/form/support-request/) if you would like assistance setting this up.

## Multicore (Threaded)

Shared-memory programs can use multiple cores but they must be physically located on the _same_ node.  The appropriate Slurm option in this case is `-c` (equivalent to `cpus-per-task`).  Shared memory programs use _threading_ of one form or another.  

Example Slurm script for a threaded program:
{{< code-download file="/notes/slurm-from-cli/scripts/multicore.slurm" lang="bash" >}}

## Multinode (MPI)

In this type of parallelism, each process runs independently and communicates with others through a library, the most widely-used of which is MPI.  Distributed memory programs can run on single or multiple nodes and often can run on hundreds or even thousands of cores.  For distributed-memory programs you can use the `-N` option to request a number of nodes, along with `ntasks-per-node` to schedule a number of processes on each of those nodes.

{{< code-download file="/notes/slurm-from-cli/scripts/multinode.slurm" lang="bash" >}}

#### Hybrid
Some codes can run with distributed-memory processes, each of which can run in threaded mode.  For this, request `--ntasks-per-node=NT` and `cpus-per-task=NC`, keeping in mind that the total number of cores requested on each node is then $NT \times NC$.

{{< code-download file="/notes/slurm-from-cli/scripts/hybrid.slurm" lang="bash" >}}
