---
title: Example Slurm Script - PyTorch
date: 2025-02-23-19:06:23Z
type: docs 
weight: 3200
menu: 
    llms-hpc:
        parent: Slurm Scripts
---

Below is an example Slurm script which runs a task using the Pytorch framework and GPUs on the cluster.

```bash
#!/bin/bash
#SBATCH -A mygroup
#SBATCH -p gpu
#SBATCH --gres=gpu:1
#SBATCH -c 1
#SBATCH -t 00:01:00
#SBATCH -J pytorchtest
#SBATCH -o pytorchtest-%A.out
#SBATCH -e pytorchtest-%A.err

# load software
module purge
module load apptainer pytorch

# run code
apptainer run --nv $CONTAINERDIR/pytorch-2.4.0.sif pytorch_example.py

```

Set up resources:
* -A: allocation
* -p: partition
* --gres=gpu:1 : use 1 gpu
* -c: number of cores
* -t: time limit
* -J: job name
* -o: standard output file (%A is the job #)
* -e: standard error file (%A is the job #)

The default command defined in each container is “python” so using “run” basically executes “python file_name.py”.

The load software and run code lines are what a user would use to run their script at the command line.

[TensorFlow example](https://www.rc.virginia.edu/userinfo/rivanna/software/tensorflow/#tensorflow-slurm-jobs) 


