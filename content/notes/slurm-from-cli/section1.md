---
title: I - HPC and Resource Management
date: 2023-12-11-14T00:11:14Z
type: docs 
weight: 10
toc: true
menu: 
    slurm-from-cli:
---

## Resources and Partitions

An HPC **job** is a description of the resources required, any preparatory steps such as loading [modules](https://www.rc.virginia.edu/userinfo/rivanna/software/modules/) or otherwise setting up an environment, and the commands to run the software, along with any postprocessing that may be appropriate.

The job is specified through a special form of script often called a _batch script_.  Usually it is written in `bash`.

Resources include the quantity of time requested, the amount of memory, the number of cores per node, and if appropriate the number of nodes or the number and/or architecture of GPU.

In the abstract, a _queue_ is a sequence of jobs to be prioritized and handled. In a cluster, a queue, which Slurm calls a **partition**, is implemented with a group of compute nodes that provide a particular set of resources.

Slurm is a software package that manages the resources of the cluster and balances the demands of many competing job requests.  It consists of a _workload manager_, often called a scheduler, and a _slurmd_ "daemon" which runs on each node and handles the execution and monitoring of the jobs.

## Cores, Nodes, and Tasks

### Hardware

The Slurm model is a cluster consisting of a number of **nodes**.  Each node is a separate server.  These servers are similar to an ordinary desktop computer, but are more reliable and usually provide more memory and cores that an ordinary desktop.

A **core** is a computing unit. It is part of a **cpu**.  

{{< alert >}}
Slurm began when cpus had only one core each. Beginning around 2005, cpus began to be divided into multiple cores.  But Slurm still refers to cores as "cpus."
{{< /alert >}}

**Memory** refers to _random-access memory_.  It is not the same thing as storage.  If a process reports running out of memory, it means RAM memory. Running out of disk space will result in a different error.

For more details about the structure of a computational cluster, see our [introduction](https://learning.rc.virginia.edu/notes/rivanna-intro/).
## Processes and Tasks

A **process** can be envisioned an instance of an executable that is running on a particular computer.  Most executables run only a single process.  Some executables run _threads_ within the _root_ process.

Slurm refers to the root process as a **task**. By default, each task is assigned to one core.


## Slurm Resource Requests

SLURM refers to queues as  __partitions__ .  We do not have a default partition; each job must request one explicitly.

{{< table >}}
| Queue Name | Purpose | Job Time Limit | Max Memory / Node / Job | Max Cores / Node |
| :-: | :-: | :-: | :-: | :-: |
| standard | For jobs on a single compute node | 7 days | 375 GB | 37 |
| gpu | For jobs that can use general purpose GPU’s<br /> (A40,A100,A6000,V100,RTX3090) | 3 days | 1953 GB | 125 |
| parallel | For large parallel jobs on up to 50 nodes (<= 1500 CPU cores) | 3 days | 375 GB | 40<br /> |
| largemem | For memory intensive jobs | 4 days | 768 GB<br />1 TB | 45 |
| interactive | For quick interactive sessions (up to two RTX2080 GPUs) | 12 hours | 216 GB |  37  |
{{< /table >}}

To see an online list of available partitions, from a command line type
```bash
$qlist
```

A more detailed view of the partitions and their limits is available through the command
```bash
$qlimits
```
## Batch Scripts

Jobs are described to the resource manager in the form of a _script_.  Typically this is written in the _bash_ scripting language.  Bash is the default shell on most Linux-based systems, which includes the majority of HPC systems, so it is expected to be available to interpret the script.  However, Slurm accepts scripts in other languages if the interpreter is available.  We will consider only bash scripts in this tutorial.

To prepare a job, the user writes a script. The top of the script is a _preamble_ that describes the resource requests. The rest of the script contains the instructions to execute the job. The script is then **submitted** to the Slurm system. The Slurm workload manager examines the preamble to determine the resources needed, while ignoring the rest of the script. It uses the resource request along with a **fair share** algorithm to set the priority of the job.  The job is then placed into the requested partition to wait for the resources to become available.  

Once the job starts, the slurmd daemon runs the script as an ordinary shell script. The preamble consists of _comments_ (code that is not executed by the interpreter) so they are ignored. The rest of the script must be a valid bash shell script.
