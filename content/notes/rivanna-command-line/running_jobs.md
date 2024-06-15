---
title: Running Jobs
date: 2023-12-11T21:14:11-14:00
type: docs 
weight: 2800
menu: 
    rivanna-command-line:
---

In an HPC environment, the tasks that users want to perform are generically called **jobs**.  These must not be confused with the "jobs" of the Unix `jobs` command, although the concepts are related. An HPC "job" is a combination of _resource requests_, any setup required such as loading modules, and the actual commands required to run the desired software.

HPC jobs are run on _compute nodes_, not on the interactive frontends.

In the Introduction we discussed using the OOD [job composer](/notes/rivanna-intro/features_of_ood/features_jobs_tab) to submit and monitor jobs. When working at the command line, we must use more detailed features of Slurm.  Please go through our [tutorial](/notes/slurm-from-cli) to learn how to write and submit Slurm scripts from the command line.
