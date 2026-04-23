---
title: Checking Job Status
date: 2026-04-20T02:29:07Z
type: docs 
weight: 1480
menu: 
    rio-intro:
        parent: Checking Queued Jobs
---

`scontrol` returns information for a running job:

```bash
scontrol show job <jobid>
```

`sacct` will return accounting information about completed and queued jobs. See [Slurm's documentation](https://slurm.schedmd.com/sacct.html) for a full list of options. Some common options include:

- Use the option `-o <options,list>` to customize the output
- Use the option `-S <yyyy-mm-dd>` to inspect jobs finished after the given start date
- Use the option `-j <jobid>` to inspect a particular job

`seff` will return information about the utilization (called the "efficiency") of core and memory:

```bash
seff <jobID>
```


