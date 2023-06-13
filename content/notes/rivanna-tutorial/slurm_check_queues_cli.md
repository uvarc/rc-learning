---
title: Checking Jobs from the Command Line
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 780

menu:
  rivanna-tutorial:
    parent: The Slurm Resource Manager
---

Regardless of how the job was submitted, through the Job Composer or at a command line, you can use the Open OnDemand Job Viewer or you can use command-line tools to monitor your job.

If you are using a terminal you can type

```bash
squeue
```
to view all jobs, and 
```bash
squeue -u mst3k
```
with your user ID, to view only your jobs.  To view a specific queue
```bash
squeue -p partition
```

Information about a running job, use `sacct` with the job ID.
```bash
sacct -j 12345
```
