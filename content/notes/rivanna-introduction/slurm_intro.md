---
title: The Slurm Resource Manager
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 700

menu:
  rivanna-introduction:
    name: The Slurm Resource Manager
---

Interactive Apps in Open OnDemand are convenient but have limitations. For long jobs they may be inconvenient and sometimes unrealistic. In other cases, we may wish to submit many jobs at once, which is awkward with interactive work. Finally, if we are running Jupyter notebooks, a network interruption could kill the process we are trying to run.  For all of these cases, the solution is to run _batch_ jobs. They will run in the background and you can return later to get the output.

To run batch jobs, a user logs in to one of the frontends, or to Open OnDemand.  A _job request_ is prepared for the compute nodes.  The job request contains all the information required to request resources (cores, memory, GPU hardware, and so forth) and the instructions for carrying out the computation.  For batch runs, this request takes the form of a _job script_, which is usually a specialized form of a `bash` script.  The script is then submitted to the Slurm system. 

