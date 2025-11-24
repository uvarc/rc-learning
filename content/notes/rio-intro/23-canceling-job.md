---
title: Canceling a Job
date: 2025-11-12-03:53:56Z
type: docs 
weight: 1450
menu: 
    rio-intro:
        parent: Slurm
---


To delete a job from the queue, use the `scancel` command with the job ID number at the command line prompt: `-bash-4.2$ scancel 18316`

To cancel all your jobs, run this command: `-bash-4.2$ scancel –u $USER`

We use `scontrol` to print information of a running job: `scontrol show job <jobid>`

**More Commands:**

`sacct` will return accounting information about your job. See [Slurm's](https://slurm.schedmd.com/sacct.html)[ ](https://slurm.schedmd.com/sacct.html)[docuementation](https://slurm.schedmd.com/sacct.html) for a full list of options.
  * Use the option `–j <jobid>` to inspect a particular job.


`seff` will return information about the utilization (called the “efficiency”) of core and memory.

