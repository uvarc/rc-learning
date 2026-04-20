---
title: Submitting an Interactive Slurm Job
date: 2026-04-20T02:29:07Z
type: docs 
weight: 1450
menu: 
    rio-intro:
        parent: Slurm
---


To submit an interactive SLURM job to the queue, use the `ijob` command at the command line prompt.

For example, if you want to run an interactive application on a compute node in the standard queue using one CPU, we can submit it as follows:

```bash
-bash-4.1$ ijob -c 1 -p standard -A ivy-hip-name -t 06:00:00

salloc: Pending job allocation 21640112

salloc: job 21640112 queued and waiting for resources

salloc: job 21640112 has been allocated resources

salloc: Granted job allocation 21640112

srun: Step created for job 21640112

udc-aw34-21c0-teh1m$
```