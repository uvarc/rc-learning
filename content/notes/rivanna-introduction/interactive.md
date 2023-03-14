---
title: Interactive Apps with Open OnDemand
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 600

menu:
  rivanna-introduction:
    name: Interactive Apps with Open OnDemand
---

Open OnDemand's File Explorer and Terminal, along with the FastX Web interface, can be used to prepare work for the cluster. This includes transferring and editing files, looking at output, and so forth.  However, the compute nodes must be used for the actual computations.  A large, multi-user system like Rivanna must be managed by some form of _resource manager_ to ensure equitable access for all users.  Research Computing uses the [Slurm](https://slurm.schedmd.com/) resource manager.  Resource managers are also often called _queueing systems_.  Users submit _jobs_ to the queueing system. A process called a _scheduler_ examines the resource requests in each job and assigns a priority. The job then waits in a _queue_, which Slurm calls a _partition_, until the requested resource becomes available.  A partition is a set of compute nodes with a particular set of resources and limits. On Rivanna there are partitions for single-node jobs, multiple-node jobs, GPU jobs, and some other dedicated partitions.

New users may find it easiest to use Open OnDemand to run jobs on the cluster.  Open OnDemand offers an easy way to run _interactive_ jobs.  With an interactive job, you are logged in directly to a compute node and can work as if it were a frontend.  Please keep in mind that an interactive job terminates when the time limit you specify expires, unless you explicitly end the session.
