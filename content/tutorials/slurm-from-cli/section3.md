---
date: "2023-12-11"
title: "III - Jobs on the Cluster"
weight: 30
---

## Running Jobs from Scratch

We recommend that you run your jobs out of your /scratch directory.
    * Your personal /scratch/mst3k folder has much more storage space than your home directory. 
    * /scratch is on a Weka filesystem, a storage system designed specifically for fast access.
    * /scratch is connected to the compute nodes with Infiniband, a very fast network connection.

{{< alert >}}
The scratch system is not permanent storage, and files older than 90 days will be marked for deleting (purging). You should keep copies of your programs and data in more permanent locations such as your home directory, leased storage such as /project or /standard, or on your lab workstation. After your jobs finish, copy the results to permanent storage.
{{< /alert >}}

**Exercise 3**

Move or copy the hello.slurm script and the hello.py script to the new folder you created in your scratch directory in Exercise 2.  Submit hello.slurm.

## Submitting a Job

Once we have navigated to the desired working directory in a terminal window, we use the `sbatch` command to submit the job. This assumes that your Slurm script is located in the current working directory.

```bash
sbatch myjob.slurm
```
The system returns a JOBID.

We do not make the script executable.  The system handles that.

```bash
$sbatch myjob.slurm
Submitted batch job 36805
```

Always remember that you submit your **job script** and not your executable or interpreter script.

## Monitoring a Job

Once submitted, we can monitor our jobs.

### Graphical Interface

The Open OnDemand Job Viewer (Jobs tab&rarr;Active Jobs) shows a Web-based view of jobs.  You can switch the dropdown between "All Jobs" and "Your Jobs."  You can also use the Filter textbox to select jobs by partition or another criterion.  In the Filter textbox you can enter multiple strings, which acts as "and."

Clicking the right-pointing arrow on the left side will cause a dropdown box to appear that will show the job status (Pending, Running, Completed) along with much other useful information.

Remember that this is a Web page and you will need to reload it in order to see changes in status.

### Command Line

We use the `squeue` command to check on jobs from the terminal.

```bash
$squeue
```
This shows all jobs.  To narrow that down we can use the `-u` (user) option or the `-p` (partition) option.

```bash
$squeue -u mst3k
$squeue -p gpu
```

Job status is indicated by 
  * PD	pending
  * R	running
  * CG	exiting

```no-highlight
JOBID PARTITION     NAME     USER	  ST    TIME  NODES  NODELIST(REASON)

36805  standard   myjob.sl  mst3k    R    1:45    1     udc-aw38-34-l
```

Jobs should rarely be observed in the `CG` state. If they are caught in that state they cannot be canceled by the user.  Exiting jobs will not charge for the time spent in that state.

For more information on a running job, similar to what you can see from the OOD Job Viewer, use the `scontrol` command.
```bash
scontrol show job <jobid>
```

## Deleting a Job

### Open OnDemand

From the Job Viewer find your jobs.  If the job is pending or running, a red trash-can icon will appear under the "Actions" header.  Click the icon.  A dialog box will appear asking you to confirm the cancellation.

### Command Line

To cancel a job use the `scancel` with the job ID. You can use `squeue -u $USER` to obtain your job IDs, but you must know the JID of the specific job you wish to cancel.

```bash
$scancel 36805 #jobID
```

Be aware that if a job fails due to a system failure the time will not be charged, but if you cancel your job, or it fails due to inadequate resource request, your allocation will be charged for the time expended.

**Exercise 4**

Write a Slurm script that requests 30 minutes of time. Submit a job that will run for at least 30 minutes. It can be some software you use; if you do not have anything set up yet, write the preamble and then add the line
```bash
sleep 30m
```
as the command.  You won't need to request a specific amount of memory. Submit this script and monitor your job's status.  Once it starts, let it run for a few minutes, then cancel it.

{{< spoiler text="Example script" >}}
{{< code-download file="/tutorials/slurm-from-cli/scripts/slow.slurm" lang="bash" >}}
{{< /spoiler >}}

## Examining Your Utilization

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

If your memory utilization is low and you have requested a specified amount, use `sacct -o` with at least the MaxRSS field to double-check. If you do not need as much memory as you thought, you may be able to save SUs and have a shorter queue wait time if you decrease it. 

## Stream Output in Slurm

When running a program interactively, any output to the Unix [standard streams](https://learning.rc.virginia.edu/tutorials/unix-tutorial/unix_tutorial_3/) will be printed directly to the user's console window.  However, programs running under the control of Slurm will not have a console attached. 

By default, SLURM redirects both standard output and standard error to a file called `slurm-<jobid>.out`.

You can change the name of this file with the `-o` or `--output` option in your script.

```bash
#SBATCH --output=<filename>
```
or
```bash
#SBATCH -o <filename>
```

You can also separate standard-error output. Even if your program does not use standard error (not many do), Slurm uses it, so you may wish to keep that output distinct.
```bash
#SBATCH --error=<filename>
```
or
```bash
#SBATCH -e <filename>
```

Text from standard input must be redirected from a file in your command line in the script.
```bash
./myexec < myinput.txt
```

As an alternative to the Slurm options, you can also redirect standard output in the usual Unix manner if you prefer.
```bash
./myexec < myinput.txt > myoutput.dat
```
