---
title: Canceling a Job
date: 2026-04-20T02:29:07Z
type: docs 
weight: 1470
menu: 
    rio-intro:
        parent: Checking Queued Jobs
---

To delete a job from the queue, use the `scancel` command with the job ID number at the command line prompt:

```bash
scancel 18316
```

To cancel all your jobs, run this command:

```bash
scancel -u $USER
```