---
title: Slurm Partitions
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 730

menu:
  rivanna-tutorial:
    parent: The Slurm Resource Manager
---
To see a list of available partitions, from a terminal type

```bash
qlist
```

{{< code file="/notes/rivanna-tutorial/snippets/qlist_out.txt" lang="plaintext" >}}

The resources available in each partition are as follows.  Rivanna is a heterogeneous system, so available cores and memory may vary within one partition.

{{< table >}}
| Queue Name | Purpose | Job Time Limit | Memory / Node | Cores / Node |
| :-: | :-: | :-: | :-: | :-: |
| standard | For jobs on a single compute node | 7 days | 256 GB/384 GB/768 GB | 28/40/40 |
| gpu | For jobs that can use general purpose graphical processing units (GPGPUs)/(K80, P100, V100) | 3 days | 256 GB/384 GB/1 TB | 28/40/128 |
| parallel | For large parallel jobs on up to 25 nodes (<=1000 cores over all jobs) | 3 days | 384 GB | 40 |
| largemem | For memory intensive jobs (<=16 cores/node) | 4 days | 1 TB | 16 |
| dev | To run jobs that are quick tests of code (<=2 nodes, 8 cores/node) | 1 hour | 128 GB |  4  |
{{< /table >}}

