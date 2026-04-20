---
title: Slurm Script Example
date: 2026-04-20T02:29:07Z
type: docs 
weight: 1430
menu: 
    rio-intro:
        parent: Slurm
---




* A Slurm script is a bash script with Slurm directives (#SBATCH) and command-line instructions for running your program.


```bash
#!/bin/bash
#SBATCH --nodes=1              #total number of nodes for the job
#SBATCH --cpus-per-task=1      #total number of cores for the job
#SBATCH --time=1-12:00:00      #amount of time for the whole job
#SBATCH --partition=standard   #the queue/partition to run on
#SBATCH --account=ivy-hip-name #name of your Ivy group


module purge
module load goolf R  #load modules that my job needs
Rscript myProg.R     #command-line execution of my job
```