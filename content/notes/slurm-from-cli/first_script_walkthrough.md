---
title: The Hello.slurm Script
date: 2023-12-11-14:11:14Z
type: docs 
weight: 800
menu: 
    slurm-from-cli:
---

The first line says that this script should be run by the `bash` interpreter.
```bash
#!/bin/bash
```

The lines starting with `#SBATCH` are the resource requests.  They are called "pseudocomments" since they have meaning to Slurm.  There must be no space between `#` and `SBATCH` and the string must start in the first column of the line. 

```bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=32000 # mb total memory
#SBATCH –-time=1-12:00:00
#SBATCH --partition=standard
#SBATCH --account=myalloc
```
Here we are requesting
  * 1 node, 1 task
  * 32GB of memory (measured in MB). Strictly speaking this will be "Gibibyes."
  * 1 day and 12 hours of running time.
  * The standard partition (queue).  A partition must be specified.
  * The account (allocation) group `rivanna-training`

The next lines set up the environment to run our job.
```bash
module purge
module load anaconda
```

It is good practice to purge all modules first, since Slurm "remembers" any modules set in the environment where the script is launched.  Next we load the module we need to run our program, the Python distribution Anaconda.

Finally, we execute our job.
```bash
python hello.py
```

We have chosen to name this script `hello.slurm`, but it can have any name.

**Exercise 1**

Download the hello.slurm and hello.py scripts. Transfer them to the cluster by whatever means you wish.  Modify the Slurm script to use your own allocation group name.
