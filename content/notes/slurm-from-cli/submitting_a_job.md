---
title: Submitting a Job
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1200
menu: 
    slurm-from-cli:
---

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

**Exercise 3**

Move or copy the hello.slurm and hello.py scripts into a new folder in your home directory.  Submit the job.
