---
title: Interactive Jobs
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1900
menu: 
    slurm-from-cli:
---

Most HPC sites, including UVa's, restrict the memory and time allowed to processes on the frontend.  Most jobs can be submitted through the _batch_ system we have been discussing, but sometimes more interactive work is required.  For example
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
Never issue an sbatch command from within an interactive job (including OOD jobs).  The sbatch command must be used only to submit jobs from a frontend.
{{< /warning >}}
