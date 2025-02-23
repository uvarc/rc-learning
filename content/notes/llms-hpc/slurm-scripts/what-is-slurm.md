---
title: What is a Slurm Script?
date: 2025-02-23-19:06:23Z
type: docs 
weight: 3150
menu: 
    llms-hpc:
      name: Slurm Scripts
---


* HPC environments are generally shared resources among a group of users.
* In order to manage user jobs, we use Slurm, a resource manager for Linux clusters.
  * This includes deciding which jobs run, when those jobs run, and which node(s) they run on.
* A Slurm script gives Slurm the information it needs to run a job.
  * Computational resources
  * Necessary software
  * Command(s) to execute the code file

---

From our website: “Jobs are submitted to the Slurm controller, which queues them until the system is ready to run them. The controller selects which jobs to run, when to run them, and how to place them on the compute node or nodes, according to a predetermined site policy meant to balance competing user needs and to maximize efficient use of cluster resources.”

