---
title: Deleting a Job
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1400
menu: 
    slurm-from-cli:
---

## Open OnDemand

From the Job Viewer find your jobs.  If the job is pending or running, a red trash-can icon will appear under the "Actions" header.  Click the icon.  A dialog box will appear asking you to confirm the cancellation.

## Command Line

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
{{< code-download file="/notes/slurm-from-cli/scripts/slow.slurm" lang="bash" >}}
{{< /spoiler >}}
