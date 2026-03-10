---
title: DL Slurm Scripts
date: 2024-06-28T01:51:43Z
type: docs 
weight: 900
toc: true
menu: 
    deep-learning-hpc:
---


## Introduction to Slurm Scripts

HPC environments are generally shared resources among a group of users. In order to manage user jobs, we use Slurm, a resource manager for Linux clusters. This includes deciding which jobs run, when those jobs run, and which node(s) they run on.
* A Slurm script gives Slurm the information it needs to run a job (i.e computational resources, necessary software, and command(s) to execute the code file)


Jobs are submitted to the Slurm controller, which queues them until the system is ready to run them. The controller selects which jobs to run, when to run them, and how to place them on the compute node or nodes, according to a predetermined site policy meant to balance competing user needs and to maximize efficient use of cluster resources

More information about UVA's Slurm Job Manager can be found [here](https://www.rc.virginia.edu/userinfo/rivanna/slurm/).


## Example Pytorch Slurm Script

```bash
#!/bin/bash               
# Set Up Resources
#SBATCH -A mygroup                # -A: allocation
#SBATCH -p gpu                    # -p: partition 
#SBATCH --gres=gpu:1              # --gres=gpu:1 :use 1 gpu
#SBATCH -c 1                      # -c: number of cores
#SBATCH -t 00:01:00               # -t: time limit
#SBATCH -J pytorchtest            # -J: job name
#SBATCH -o pytorchtest-%A.out     # -o: standard output file (%A is the job #)
#SBATCH -e pytorchtest-%A.err     # -e: standard error file (%A is the job #)

module purge                          # Load Software
module load apptainer pytorch/2.0.1  

apptainer run --nv $CONTAINERDIR/pytorch-2.0.1.sif pytorch_example.py  # Run Code
```


## Example TensorFlow/Keras Slurm Script

```bash
#!/bin/bash
#SBATCH -A mygroup
#SBATCH -p gpu
#SBATCH --gres=gpu:1
#SBATCH -c 1
#SBATCH -t 01:00:00
#SBATCH -J tftest
#SBATCH -o tftest-%A.out
#SBATCH -e tftest-%A.err

module purge
module load apptainer tensorflow/2.13.0

apptainer run --nv $CONTAINERDIR/tensorflow-2.13.0.sif tf_example.py
```

## More Slurm Options

* To request a specific amount of memory per node:
  * For example, `--mem=64G`
  * Units are given with a suffix (K, M, G, or T).  If no unit is given, megabytes is assumed.
* Other options available at [https://slurm.schedmd.com/sbatch.html](https://slurm.schedmd.com/sbatch.html)
