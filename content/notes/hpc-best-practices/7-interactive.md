---
title: VII - Interactive Computing
date: 2025-06-14-14:47:30Z
type: docs 
weight: 350
toc: true
menu: 
    hpc-best-practices:
---

__Interactive HPC computing__ involves *real-time* user inputs to perform tasks on a set of compute node(s) including:
* Code development, real-time data exploration, and visualizations
* Used when applications have large data sets or are too large to download to local device, software is difficult to install, etc.
* User inputs come via command line interface or application GUI (Jupyter Notebooks, Matlab, R-studio)
* Actions performed on remote compute nodes as a result of user input or program out

## Interactive Jobs

Most HPC sites, including UVA's, restrict the memory and time allowed to processes on the frontend. The basic SLURM command to request interactive resources is `salloc`. However, it requires several options to work well, so we have a local script called `ijob`. `ijob` takes the same arguments as the SLURM command `salloc`.
```bash
ijob -c 1 -A myalloc -t <time> --mem <memory in MB> -p <partition> -J <jobname>
```

`ijob` is a wrapper around the SLurm commands `salloc` and `srun`, set up to start a bash shell on the remote node. The options are the same as the options to `salloc`, so most commands that can be used with `#SBATCH` can be used with `ijob`. The request will be placed into the queue specified:
```bash
$ ijob -c 1 -A mygroup -p standard --time=1:00:00:00
salloc: Pending job allocation 25394
salloc: job 25394 queued and waiting for resources
```

There may be some delay for the resource to become available.
```bash
salloc: job 25394 has been allocated resources
salloc: Granted job allocation 25394
```

For all interactive jobs, the allocated node(s) will remain reserved as long as the terminal session is open, up to the walltime limit, so it is extremely important that users exit their interactive sessions as soon as their work is done so that the user is not charged for unused time.


## Application Examples

Jupyter-lab
```bash
module load gcc jupyter_conda/.2020.11-py3.8
jupyter-lab &
```

Rstudio
```bash
module load goolf/11.2.0_4.1.4 R/4.2.1 rstudio
rstudio &
```

MATLAB
```bash
module load matlab/R2023a
matlab &
```

## Constructing Resource-Efficient SLURM Scripts

Monitoring Rivanna queues: `qlist`

{{< figure src="/notes/hpc-best-practices/img/qlist.png" width=70% height=70% >}}

Monitoring specific queues: `qlist -p`

{{< figure src="/notes/hpc-best-practices/img/qlist-p1.png" width=70% height=70% >}}

Monitoring specific queues: `qlist -p`

{{< figure src="/notes/hpc-best-practices/img/qlist-p2.png" width=70% height=70% >}}

Partition node usage display:

{{< figure src="/notes/hpc-best-practices/img/display.png" width=70% height=70% >}}

Want to know the strategy for submitting jobs to busy queues? Request fewer cores, less time, or less memory (and corresponding cores). It's also important to know exactly what compute/memory resources your job needs, as detailed in the `seff` output.

