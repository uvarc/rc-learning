---
title: Running Cutadapt with Slurm
date: 2025-08-23T03:19:53Z
date: 2025-08-23-03:19:53Z
type: docs 
weight: 1100
menu: 
    bioinfo-intro:
        parent: Sequence Processing & Alignment      
---

As an alternative to running Cutadapt interactively, you can submit a job on Rivanna using a Slurm script. 

Sample Slurm script: `cutadapt_slurm_submit.sh`

```bash
#!/bin/bash
#SBATCH -A hpc_training              # account name
#SBATCH -p standard                  # partition/queue
#SBATCH --nodes=1                    # number of nodes
#SBATCH --ntasks=1                   # 1 task
#SBATCH --cpus-per-task=1            # total cores per task
#SBATCH -t 00:20:00                  # time limit: 20 min
#SBATCH -J cutadapt-test             # job name
#SBATCH -o cutadapt-test-%A.out      # output file
#SBATCH -e cutadapt-test-%A.err      # error file

module purge  # good practice to purge all modules

module load cutadapt/4.9

cd /[your working directory]

mkdir fastqc-out-trimmed

# enter all sequences, loop around large dataset
__cutadapt__  -a CTGTCTCTTATACACATCT -o SRR2584866-trimmed_1.fastq  SRR2584866_1.fastq

```


