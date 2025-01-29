---
title: IV - Specialized Jobs
date: 2023-12-11-14T00:11:14Z
type: docs
toc: true 
weight: 40
menu: 
    slurm-from-cli:
---

## Environment Variables

An _environment variable_ describes something about your working environment.  Some of them you can set or modify; others are set by the system. To see what is currently set in your terminal, run
```bash
$printenv
```

To set an environment variable yourself, use the `export` command.
```bash
$export VAR=value
```

When your job starts, SLURM will initialize several environment variables.  Many of them correspond to options you have set in your `SBATCH` preamble. Do not attempt to assign to any variable beginning with SLURM_

Some variables that you may wish to examine or use in your scripts:

{{< table >}}
|  Variable   |  Value  |
| ---------   |  -----  |
| SLURM_SUBMIT_DIR | Directory from which the job was submitted |
| SLURM_JOB_NODELIST | List of the nodes on which the job is running |
| SLURM_JOB_ID |  The numerical ID of the job |
| SLURM_NUM_TASKS |  The number of tasks (obtained from --ntasks) |
| SLURM_NTASKS_PER_NODE |  The number of tasks per node |
| SLURM_CPUS_PER_TASK |  The number of cpus (cores) assigned to each task |
{{< /table >}}

## Interactive Jobs

Most HPC sites, including UVa's, restrict the memory and time allowed to processes on the login nodes.  Most jobs can be submitted through the _batch_ system we have been discussing, but sometimes more interactive work is required.  For example
1. Jobs that must be or are best run through a graphical interface,
2. Short development jobs,
3. "Computational steering" in which a program runs for an interval, then the output is examined and parameters may be adjusted.

For most of these cases, we strongly recommend the use of the Open OnDemand [Interactive Applications](/notes/rivanna-intro/interactive_apps/interactive/).  Jupyterlab is available to run notebooks.  Rstudio and Matlab Desktop are also available to run through this interface.  For more general work, including command-line options, the Desktop is usually the best option. It provides a basic terminal, but also access to other applications should they be needed.

For general-purpose interactive work with graphics, please use the Open OnDemand Desktop.  The X11 service that Linux uses for graphics is very slow over a network.  Even with a fast connection between two systems, the Desktop will perform better since the X11 server process and the programs that use it are running on the same computer.

If you must use a basic terminal for an interactive job, you must first use the command `salloc`.  This is the general Slurm command to request resources. This would be followed by `srun` to launch the processes.  However, this is complex and requires knowledge of the options, so we have provided a local "wrapper" script called `ijob`.

ijob takes options similar to those used with `SBATCH`, most of which are actually arguments to `salloc`.

```bash
$ijob –c 1 –A myalloc -t <time> --mem <memory in MB> -p <partition> -J <jobname>
```

When the job starts you will be logged in to a bash shell in a terminal on the compute node.

{{< warning >}}
Never issue an sbatch command from within an interactive job (including OOD jobs).  The sbatch command must be used only to submit jobs from a login node.
{{< /warning >}}

## Multicore and Multinode Jobs

One of the advantages of using a high-performance cluster is the ability to use many cores and/or nodes at once.  This is called _parallelism_.  There are three main types of parallelism.

You should understand whether your program can make use of more than one core or node before you request multiple cores and/or nodes. Special programming is required to enable these capabilities.  Asking for multiple cores or nodes that your program cannot use will result in idle cores and wasted SUs, since you are charged for each core-hour. The [`seff`](/notes/slurm-from-cli/section3/#seff) command can help with this.

### High Throughput Serial Parallelism

High throughput parallelism is when many identical jobs are run at once, each on a single core.  Examples can include Monte-Carlo methods, parameter searches, image processing on many related images, some areas of bioinformatics, and many others.  For most cases of this type of parallelism, the best Slurm option is a [job array](/notes/slurm-from-cli/section4/#job-arrays).

When planning a high-throughput project, it is important to keep in mind that if the individual jobs are very short, less than roughly 15-30 minutes each, it is very inefficient to run each one separately, whether you do this manually or through an array.  In this case you should group your jobs and run multiple instances within the same job script.  Please [contact us](https://www.rc.virginia.edu/form/support-request/) if you would like assistance setting this up.

### Multicore (Threaded)

Shared-memory programs can use multiple cores but they must be physically located on the _same_ node.  The appropriate Slurm option in this case is `-c` (equivalent to `cpus-per-task`).  Shared memory programs use _threading_ of one form or another.  

Example Slurm script for a threaded program:
{{< code-download file="/notes/slurm-from-cli/scripts/multicore.slurm" lang="bash" >}}

### Multinode (MPI)

In this type of parallelism, each process runs independently and communicates with others through a library, the most widely-used of which is MPI.  Distributed memory programs can run on single or multiple nodes and often can run on hundreds or even thousands of cores.  For distributed-memory programs you can use the `-N` option to request a number of nodes, along with `ntasks-per-node` to schedule a number of processes on each of those nodes.

{{< code-download file="/notes/slurm-from-cli/scripts/multinode.slurm" lang="bash" >}}

### Hybrid MPI plus Threading
Some codes can run with distributed-memory processes, each of which can run in threaded mode.  For this, request `--ntasks-per-node=NT` and `cpus-per-task=NC`, keeping in mind that the total number of cores requested on each node is then $NT \times NC$.

{{< code-download file="/notes/slurm-from-cli/scripts/hybrid.slurm" lang="bash" >}}

## GPU Jobs

We have a dedicated partition with nodes that are equipped with Graphical Processing Units (GPUs). Code must be built to take advantage of GPU resources. If the packages and your code are not written to use GPU resources, the GPUs will remain idle and you will be charged the SUs for them. Some popular Python packages that you could use are Pytorch or TensorFlow. We have some material on how to get started with using them in a Deep Learning setting.

GPU job scripts are similar to CPU scripts, but do require the addition of the --gres=gpu option. Example Slurm script requesting 1 GPU:

{{< code-download file="/notes/slurm-from-cli/scripts/gpu.slurm" lang="bash" >}}

The example script uses a Pytorch container that we have installed on the system. The Python command it is performing prints True if CUDA (A set of GPU libraries) is visible to the GPU. This is an important indicator if Pytorch can use communicate with the GPU. If this prints False, something is wrong with the setup and requires attention.

We have several different GPU types equipped on our nodes each offering varying amounts of memory. See our website for Hardware Specifications. In the example above, Slurm will choose whatever GPU is available. If you are working with larger models you may find that you need a GPU with more memory. To request a specific GPU, you add it to the --gres Slurm option. If a GPU type has multiple options (for instance, we offer 40GB and 80GB A100 GPUs), there will be a --constraint you can use to specify even further. Example Slurm script requesting 1 80GB A100 GPU node:

{{< code-download file="/notes/slurm-from-cli/scripts/gpua100.slurm" lang="bash" >}}

Note that the more specific your request is, the longer you will likely have to wait for the resource to be available.

## Job Arrays

Many similar jobs can be submitted simultaneously through _job arrays_. There are some restrictions:

* It must be a batch job.
* Job arrays should be explicitly named with `-J`
* It is generally prudent to separate stdout and stderror with `-o` and `-e`

A job array is submitted with `sbatch --array=<range>`, where `range` is two digits separated by a hyphen.
```bash
$sbatch --array=0-30 myjobs.sh
```
An increment can be provided
```bash
$sbatch --array=1-7:2 myjobs.sh
```
This will number them 1, 3, 5, 7

It is also possible to provide a list
```bash
$sbatch --array=1,3,4,5,7,9 myjobs.sh
```

Each job will be provided an environment variable `SLURM_ARRAY_JOB_ID` and
each task will be assigned a `SLURM_ARRAY_TASK_ID`.  The ARRAY_JOB_ID is the overall jobid, whereas the `ARRAY_TASK_ID` will take on the values of the numbers in the specified range or list.

Slurm also provides two variables `%A` (global array ID) and `%a` (array task ID) which can be used in the `-o` and `-e` options.  If they are not used, then the different tasks will attempt to write to the same file, which can result in garbled output or file corruption, so please use them if you wish to redirect streams with those options.

To prepare a job array, set up any input files using appropriate names that will correspond to the numbers in your _range_ or _list_, e.g.
```
myinput.0.in
myinput.1.in
...
myinput.30.in
```
You would submit a job for the above files with
```bash
$sbatch --array=0-30
```
In your Slurm script you would use a command such as
```bash
python myscript.py myinput.${SLURM_ARRAY_TASK_ID}.in
```

The script should be prepared to request resources for _one_ instance of your program.

Complete example array job script:
{{< code-download file="/notes/slurm-from-cli/scripts/array.slurm" lang="bash" >}}

To cancel an entire array, cancel the global ID
```bash
scancel 1283839
```
You can also cancel individual tasks
```bash
scancel 1283839_11
```

## Useful Commands

When you submit a job and it doesn't start or fails for an unknown reason it could be due to restraints in your account. This could include running out of storage space or SUs on your allocation. Additionally, it's useful to see how busy the queue is. The following subsections highlight how to identify these problems.

### Allocations

Sometimes it’s useful to check how many SUs are still available on your allocation. The `allocations` command displays information on your allocations and how many SUs are associated with them:

```
$ allocations
Account                      Balance        Reserved       Available                
-----------------          ---------       ---------       ---------
hpc_training                 1000000               0          999882 
```

running `allocations -a <allocation_name>` provides even more detail on when the allocation was last renewed and its members. E.g.

```
$ allocations -a hpc_training
Description StartTime           EndTime    Allocated   Remaining  PercentUsed Active 
----------- ------------------- ---------- ----------- ---------- ----------- ------ 
new         2024-05-29 17:33:13 2025-05-29 1000000.000 999881.524        0.01 True   

Name   Active CommonName                     EmailAddress        DefaultAccount         
------ ------ ------------------------------ ------------------- ----------------------
.
.
.
```

### Storage Quota
One way to check your storage utilization is with the `hdquota` command. This command will show you how much of your home, scratch, and leased (if applicable) storage are being utilized. Below is the sample output for `hdquota`:

```
$ hdquota
Type             Location         Name                                        Size Used Avail Use%
====================================================================================================
home             /home            mst3k                                       50G   16G   35G  32%
Scratch          /scratch         mst3k                                       12T  2.0T   11T  17%
```

This is a useful command to check whether you’re running out of storage space or to see where files need to be cleaned up. For more detailed information on disk utilization you may also use the `du` command to investigate specific directories.

### Queue limits and Usage

To gain information on the different queues you can use the `qlist` command. This will show the list of partitions, their usage, and the SU charge rate. You can use `qlimits` for information on each queue’s limits.

The `sinfo` command will provide some more detailed information on the health of each queue and the number of active nodes available. These commands can be useful in diagnosing why a job may not be running, or to better understand the queue usage for more efficient job throughput. More information on hardware specifications and queue information can be found [here](https://rc.virginia.edu/userinfo/rivanna/overview/#hardware-configuration) on our website.

## Need Help

Research Computing is ready to help you learn to use our systems efficiently.  You can [submit a ticket](https://www.rc.virginia.edu/form/support-request/).  For in-person help, please attend one of our weekly sessions of [office hours](https://www.rc.virginia.edu/support/#office-hours).
