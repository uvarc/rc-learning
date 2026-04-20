---
title: Submitting a Slurm Job
date: 2026-04-20T02:29:07Z
type: docs 
weight: 1440
menu: 
    rio-intro:
        parent: Slurm
---

To submit the Slurm command file to the queue, use the `sbatch` command at the command line prompt. Note that `/home` only exists on the VM and compute nodes do not have access to it. Job submission needs to be done from `/standard/storage/`, and it is recommended to include the complete file paths in the script.

For example, if the script on the previous page is in a file named `job_script.slurm`, we can submit it as follows:

```bash
[jus2yw@ivy-tst-rc-1 ivy-tst-rc]$ sbatch job_script.slurm

Submitted batch job 18316
```