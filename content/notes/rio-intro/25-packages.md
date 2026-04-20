---
title: Python & R packages
date: 2026-04-20T02:29:07Z
type: docs 
weight: 1350
menu: 
    rio-intro:
        parent: Preinstalled Software
---

Since `/home` is not mounted on compute nodes, locally installed Python and R packages must be staged on `/standard` instead. Both languages, by default, install to a folder in `/home`. Configuration changes are necessary to use Python and R in your compute jobs on Rio.

Instructions for each are posted on our website:

- [Conda environments](https://www.rc.virginia.edu/userinfo/hpc/software/miniforge/#using-conda-environments-on-rio "Using Conda Environments on Rio page from the RC Website")
- [R libraries](https://www.rc.virginia.edu/userinfo/hpc/software/r/#submitting-jobs-to-rio "Submitting Jobs to Rio page from the RC Website")

When using interactive JupyterLab or RStudio, extra setup is not necessary.


