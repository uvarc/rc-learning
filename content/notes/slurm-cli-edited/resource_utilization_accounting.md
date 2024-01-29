---
title: Examining Your Utilization
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1500
menu: 
    slurm-from-cli:
---

When your jobs have finished, you may wish to find out how much of the resource you utilized.  Two commands can be used for this purpose, `sacct` and `seff`.

### sacct

As the name suggests, `sacct` will return accounting information about your job.  It is built-in to Slurm and does not know about local policies such as SU charges, but it will show you information about the job. It only works for jobs that have ended.

With no options it will show output for jobs run on the current date.
```bash
JobID           JobName  Partition    Account  AllocCPUS      State ExitCode 
------------ ---------- ---------- ---------- ---------- ---------- -------- 
56220974      mpi.slurm   parallel  hpc_build         10     FAILED      9:0 
56220974.ba+      batch             hpc_build          5     FAILED      9:0 
56220974.0   mpiheated+             hpc_build         10     FAILED      1:0 
56220992      mpi.slurm   standard  hpc_build         10  COMPLETED      0:0 
56220992.ba+      batch             hpc_build         10  COMPLETED      0:0 
56220992.0   mpiheated+             hpc_build         10  COMPLETED      0:0 
56221184      mpi.slurm   standard  hpc_build         10  COMPLETED      0:0 
56221184.ba+      batch             hpc_build         10  COMPLETED      0:0 
56221184.0   mpiheated+             hpc_build         10  COMPLETED      0:0 
56221192      mpi.slurm   standard  hpc_build         10  COMPLETED      0:0 
56221192.ba+      batch             hpc_build         10  COMPLETED      0:0 
56221192.0   mpiheated+             hpc_build         10  COMPLETED      0:0 
```

For a particular job, use the `-j` option.

```bash
$sacct -j 56221192
JobID           JobName  Partition    Account  AllocCPUS      State ExitCode 
------------ ---------- ---------- ---------- ---------- ---------- -------- 
56221192      mpi.slurm   standard  hpc_build         10  COMPLETED      0:0 
56221192.ba+      batch             hpc_build         10  COMPLETED      0:0 
56221192.0   mpiheated+             hpc_build         10  COMPLETED      0:0 
```

For more detail, specify the `-o` option and a list of fields. The list of available fields is returned by `sacct -e` and is lengthy. For example, if I use only one allocation I may not be interested in that field.
```bash
$sacct -o jobname,jobid,ncpus,nnodes,maxrss,state,elapsed -j 56221192
   JobName JobID             NCPUS   NNodes     MaxRSS      State    Elapsed 
---------- ------------ ---------- -------- ---------- ---------- ---------- 
 mpi.slurm 56221192             10        1             COMPLETED   00:00:34 
     batch 56221192.ba+         10        1      4824K  COMPLETED   00:00:34 
mpiheated+ 56221192.0           10        1    108800K  COMPLETED   00:00:33 
```

The output from `sacct` can be heavily customized. For more information see the [documentation](https://slurm.schedmd.com/sacct.html).

Running `sacct` puts a load on the system and can be very slow, so please use it judiciously.

### seff

The `seff` command returns information about the utilization (called the "efficiency") of core and memory.  
```bash
$seff 56221192
Job ID: 56221192
Cluster: shen
User/Group: mst3k/users
State: COMPLETED (exit code 0)
Nodes: 1
Cores per node: 10
CPU Utilized: 00:05:17
CPU Efficiency: 93.24% of 00:05:40 core-walltime
Job Wall-clock time: 00:00:34
Memory Utilized: 1.04 GB (estimated maximum)
Memory Efficiency: 1.18% of 87.89 GB (8.79 GB/core)
```

Under most circumstances, for a cpu-only job the "CPU" (core) efficiency should be around 90% or better.  Please contact us if it is significantly lower than that.  Note that seff may be relatively inaccurate for very short jobs.

Core efficiency is more problematic for GPU jobs, since the key to efficient GPU utilization is maximizing the GPU computations and minimizing CPU work. Seff does not provide a GPU utilization metric at this time, but we may be able to help you if you are concerned about GPU utilization.

If your memory utilization is low and you have requested a specified amount, use `sacct -o` with at least the MaxRSS field to double check. If you do not need as much memory as you thought, you may be able to save SUs and have a shorter queue wait time if you decrease it. 

