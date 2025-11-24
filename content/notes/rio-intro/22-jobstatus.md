---
title: Checking Job Status
date: 2025-11-12-03:53:56Z
type: docs 
weight: 1400
menu: 
    rio-intro:
      parent: Slurm
---

To display the status of only your  _active_ jobs, type: `squeue –u <your_user_id>`.

The `squeue` command will show pending jobs and running jobs, but not failed, canceled or completed job.

```bash
-bash-4.2$ squeue –u mst3k
```
Output: 

{{< table >}}
| JOBID | PARTITION | NAME | USER | ST | TIME | NODES | NODELIST(REASON) |
| :-: | :-: | :-: | :-: | :-: | :-: | :-: | :-: |
| 18316 | standard | job_sci | mst3k | R | 1:45 | 1 | udc-aw38-34-l |
{{< /table >}}

A job's status is indicated by:

`PD` – pending

`R` – running

`CG` – exiting


To display the status of all jobs, type:  `sacct –S <start_date>`.

```bash
[jus2yw@ivy-tst-rc-1 ivy-tst-rc]$ sacct –S 2025-01-01
```
Output: 
{{< table >}}
| JobID	| JobName	| Partition	| Account	| AllocCPUS | State | ExitCode|
| :-: | :-: | :-: | :-: | :-: | :-: | :-: | 
| 3104009 | RAxML_NoC+ | standard | hpc_build | 20 | COMPLETED | 0:0 |
| 3104009.bat+ | batch |  | hpc_build | 20 | COMPLETED | 0:0 |
| 3104009.0 | raxmlHPC-+ |  | hpc_build | 20 | COMPLETED | 0:0 |
| 3108537 | sys/dashb+ | gpu | hpc_build | 1 | CANCELLED+ | 0:0 |
| 3108537.bat+ | batch |  | hpc_build | 1 | CANCELLED | 0:15 |
| 3108562 | sys/dashb+ | gpu | hpc_build | 1 | TIMEOUT | 0:0 |
| 3108562.bat+ | batch |  | hpc_build | 1 | CANCELLED | 0:15 |
| 3109392 | sys/dashb+ | gpu | hpc_build | 1 | TIMEOUT | 0:0 |
| 3109392.bat+ | batch |  | hpc_build | 1 | CANCELLED | 0:15 |
| 3112064 | srun | gpu | hpc_build | 1 | FAILED | 1:0 |
| 3112064.0 | bash |  | hpc_build | 1 | FAILED | 1:0 |
{{< /table >}}

The `sacct` command lists all jobs (pending, running, completed, canceled, failed, etc.) since the specified date.

