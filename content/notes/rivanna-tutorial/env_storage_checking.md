---
title: Checking Your Storage
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 420

menu:
  rivanna-tutorial:
    parent: The Cluster Environment
---

To check how much disk space you have used in your home and scratch directories, open a Terminal through Open OnDemand or FastX, and type  <span style="color:#0070C0"> __hdquota__ </span>  at the command\-line prompt:

```bash
hdquota

Type             Location         Name                   Size Used Avail Use%
================================================================================
home             /home            mst3k                   50G   35G   16G  69%

Scratch usage for mst3k
                         Block Limits                                               |     File Limits
Filesystem Fileset    days             GB      quota      limit   in_doubt    grace |    files   quota    limit in_doubt    grace  Remarks
gpfs0      scratch     90             266      10240      12288          1     none |     9594  350000   420000        1     none 

```

The `hdquota` command will also show usage for any research project or research standard storage to which you have access.
