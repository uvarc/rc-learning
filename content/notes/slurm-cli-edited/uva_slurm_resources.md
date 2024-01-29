---
title: SLURM Resource Requests
date: 2023-12-11-14:11:14Z
type: docs 
weight: 400
menu: 
    slurm-from-cli:
---


SLURM refers to queues as  __partitions__ .  We do not have a default partition; each job must request one explicitly.

{{< table >}}
| Queue Name | Purpose | Job Time Limit | Memory / Node | Cores / Node |
| :-: | :-: | :-: | :-: | :-: |
| standard | For jobs on a single compute node | 7 days | 384 GB | 40 |
| gpu | For jobs that can use general purpose GPU’s<br /> (P100,V100,A100) | 3 days | 256 GB<br />384 GB<br />1 TB | 28<br />40<br />128 |
| parallel | For large parallel jobs on up to 50 nodes (<= 1500 CPU cores) | 3 days | 384 GB | 40<br /> |
| largemem | For memory intensive jobs | 4 days | 768 GB<br />1 TB | 16 |
| dev | To run jobs that are quick tests of code | 1 hour | 128 GB |  40  |
{{< /table >}}

To see an online list of available partitions, from a command line type
```bash
$qlist
```

A more detailed view of the partitions and their limits is available through the command
```bash
$qlimits
```
