---
title: Using More Than One Core
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 790

menu:
  rivanna-tutorial:
    parent: The Slurm Resource Manager
---

Every example we have considered so far was a _serial_ job, i.e. it ran on a single core on a single node.  However, much of the benefit of a high-performance cluster like Rivanna is the ability to utilize many cores at once.  This is called _parallelism_.

## High-Throughput Parallelism

In high-throughput parallelism, each job runs on a single core (or perhaps a small number of cores) but we want to run many of them, up to hundreds or thousands.  Most high-throughput workflows can be set up as a _job array_.  Please see our [example](https://www.rc.virginia.edu/userinfo/rivanna/slurm/#job-arrays).  When submitted, a job array receives a Slurm job ID as usual, but each subtask also has an "array task ID" associated with it.  The `${SLURM_ARRAY_TASK_ID}` is an _environment variable_.  It is assigned by Slurm when the job is initiated.  You can use it in your scripts but should not attempt to set its value.  It will take on the array task ID for each subtask when it is started.  The job script for each array task is identical; variables allow for different patterns in input/output files and such.

Job arrays should be submitted to the standard partition, unless individual array tasks can use multiple nodes.  You may submit more array tasks than are allowed to run at one time; currently the limit is 9999. Slurm will start as many as possible initially, then as an array task completes, the next in line will be started.

The OOD Job Composer has several job-array templates (search on "array").  You can copy them without submitting a job, and modify them for your needs.

## Multicore, Single Node

Some programs are able to run on more than one core but must be kept on the same node.  This is _threading_.  Our example script is [here](https://www.rc.virginia.edu/userinfo/rivanna/slurm/#threaded-jobs-openmp-or-pthreads).  You should find out how your program or script sets the number of threads; typically this is by some environment variable like `$NTHREADS`.  That should be set to `${SLURM_CPUS_PER_TASK} in your script.  The corresponding resource requests are
```bash
$SBATCH --ntasks=1
$SBATCH --cpus-per-task=8
```
The `--cpus-per-task` option can also be abbreviated
```bash
$SBATCH -c 8
```
Using this request will ensure that cores for the threads for each controlling task will be on the same node.

Keep in mind that a program must be written for threading; it does not happen automatically.

Threaded programs must use the standard partition.

## Multicore, Multinode

Some programs can utilize not only more than one core, but more than one node.  Most of these programs use the Message Passing Interface (MPI).  An example script is [available](https://www.rc.virginia.edu/userinfo/rivanna/slurm/#distributed-memory-jobs).  In this type of program, each process runs independently and communicates with the others.  Slurm regards each of these processes as a separate task, so the requests are
```bash
$SBATCH --nodes=2
$SBATCH --ntasks-per-node=40
```
This example will run a total of 80 processes.  The Slurm `srun` command should be used to launch the tasks.

Distributed programs may use the standard partition if running on a single node.  The parallel partition is available for jobs using more than one node.

## GPUs

GPUs must be requested explicitly as a resource. This is accomplished through a `gres` (generic resource) and the job must be submitted to the gpu partition.  The architecture may be specified.  More information is available [here](https://www.rc.virginia.edu/userinfo/rivanna/slurm/#gpu-computations).

