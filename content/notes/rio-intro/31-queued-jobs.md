---
title: Checking Queued Jobs
date: 2026-04-20T02:29:07Z
type: docs 
weight: 1460
menu: 
    rio-intro:
---

To display the status of only your active jobs, type:

```bash
squeue -u <your_user_id>
```

The `squeue` command will show pending jobs and running jobs, but not failed, canceled, or completed jobs. A job's status is indicated by: PD (pending), R (running), or CG (exiting).

```bash
-bash-4.2$ squeue -u mst3k

JOBID   PARTITION   NAME     USER   ST   TIME   NODES  NODELIST(REASON)

standard    job_sci  mst3k  R    1:45    1     udc-aw38-34-l
```