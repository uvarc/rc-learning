---
title: Partitions (Queues)
date: 2026-04-20T02:29:07Z
type: docs 
weight: 1420
menu: 
    rio-intro:
        parent: Slurm
---

Rio has several partitions (or queues) for job submissions. You will need to specify a partition when you submit a job. To see the partitions that are available to you, type `qlist` at the command-line prompt. 

{{< figure src=/notes/rio-intro/img/rio-intro_1.png alt="qlist output showing partitions and their total cores, free cores, jobs running and pending, and time limits" caption="qlist output" >}}

The neo partitions are exclusive to a specific group and are not for general users of Rio.

