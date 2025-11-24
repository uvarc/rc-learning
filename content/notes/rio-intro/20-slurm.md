---
title: Slurm
date: 2025-11-12-03:53:56Z
type: docs 
weight: 1000
menu: 
    rio-intro:
---

**Slurm** manages the hardware resources on the cluster (e.g. compute nodes/cpu cores, compute memory, etc.).

Slurm allows you to request resources within the cluster to run your code. It is used for submitting jobs to compute nodes from an access point (Ivy VM).

Frontends are intended for editing, compiling, and very short test runs. 

Production jobs go to the compute nodes through the resources manager.

#### Example Slurm Script

A Slurm script is a bash script with Slurm directives (#SBATCH) and command-line instructions for running your program.

```bash
#!/bin/bash
#SBATCH --nodes=1 #total number of nodes for the job
#SBATCH --ntasks-per-node=1 #how many copies of code to run
#SBATCH --time=1-12:00:00 #amount of time for the whole job
#SBATCH --partition=standard #the queue/partition to run on
#SBATCH --account=myGroupName #the account/allocation to use

module purge
module load goolf R #load modules that my job needs
Rscript myProg.R #command-line execution of my job
```

#### Submitting a Slurm Job

To submit the Slurm command file to the queue, use the `sbatch` command at the command line prompt.

`/home` only exists on VM. Compute nodes do not have access to `/home`.

Job submission needs to be done from `/standard/storage/`.

It is recommended to include the complete file paths in the script. For example, if the script on the previous page is in a file named `job_script.slurm`, we can submit it as follows:
```bash
[jus2yw@ivy-tst-rc-1 ivy-tst-rc]$ sbatch job_script.slurm
``` 

Output: 
```plaintext
Submitted batch job 18316
```

#### Submitting an Interactive Slurm Job 


To submit an interactive Slurm job to the queue, use the `ijob` command at the command line prompt.

For example, if you want to run an interactive application on a compute node in the standard queue using one cpu, we can submit it as follows:
```bash
-bash-4.1$ ijob –c 1 –p standard –A MyGroupName -t 06:00:00
```
Output: 
```plaintext
salloc: Pending job allocation 21640112
salloc: job 21640112 queued and waiting for resources salloc: job 21640112 has been allocated resources
salloc: Granted job allocation 21640112 srun: Step created for job 21640112
udc-aw34-21c0-teh1m$
```

Slurm documentation:
  * [https://www.rc.virginia.edu/userinfo/hpc/slurm/](https://www.rc.virginia.edu/userinfo/hpc/slurm/)
  * [https://slurm.schedmd.com/](https://slurm.schedmd.com/)