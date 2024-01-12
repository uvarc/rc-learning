---
title: Monitoring Jobs
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1300
menu: 
    slurm-from-cli:
---

Once submitted, we can monitor our jobs.

## Graphical Interface

The Open OnDemand Job Viewer (Jobs tab&rarr;Active Jobs) shows a Web-based view of jobs.  You can switch the dropdown between "All Jobs" and "Your Jobs."  You can also use the Filter textbox to select jobs by partition or another criterion.  In the Filter textbox you can enter multiple strings, which acts as "and."

Clicking the right-pointing arrow on the left side will cause a dropdown box to appear that will show the job status (Pending, Running, Completed) along with much other useful information.

Remember that this is a Web page and you will need to reload it in order to see changes in status.

## Command Line

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
