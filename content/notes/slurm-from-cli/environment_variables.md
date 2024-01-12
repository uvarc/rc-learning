---
title: Environment Variables
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1800
menu: 
    slurm-from-cli:
---
An _environment variable_ describes something about your working environment.  Some of them you can set or modify; others are set by the system. To see what is currently set in your terminal, run
```bash
$printenv
```

To set an environment variable yourself, use the `export` command.
```bash
$export VAR=value
```

When your job starts, SLURM will initialize several environment variables.  Many of them correspond to options you have set in your `SBATCH` preamble. Do not attempt to assign to any variable beginning with SLURM_

Some variables that you may wish to examine or use in your scripts:

{{< table >}}
|  Variable   |  Value  |
| ---------   |  -----  |
| SLURM_SUBMIT_DIR | Directory from which the job was submitted |
| SLURM_JOB_NODELIST | List of the nodes on which the job is running |
| SLURM_JOB_ID |  The numerical ID of the job |
| SLURM_NUM_TASKS |  The number of tasks (obtained from --ntasks) |
| SLURM_NTASKS_PER_NODE |  The number of tasks per node |
| SLURM_CPUS_PER_TASK |  The number of cpus (cores) assigned to each task |
{{< /table >}}

