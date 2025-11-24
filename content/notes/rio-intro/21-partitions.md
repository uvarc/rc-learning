---
title: Partitions (Queues)
date: 2025-11-12-03:53:56Z
type: docs 
weight: 1050
menu: 
    rio-intro:
        parent: Slurm
---

Rio has several partitions (or queues) for job submissions.

You will need to specify a partition when you submit a job.

To see the partitions that are available to you, type `qlist` at the command-line prompt.


{{< figure src=/notes/rio-intro/img/rio-intro_9.png caption="Command output" alt="Screenshot of terminal output displaying Slurm partition information. The table lists four queues—neo, neo-gpu, standard, and gpu—along with their total and free cores, running and pending jobs (all zero), and time limits of seven days for most partitions and three days for the GPU queue." width=75% height=75% >}}

> <small>The `neo` partitions are exclusive to a specific group and are not for general users of Rio.</small>

