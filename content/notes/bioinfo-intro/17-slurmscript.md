---
title: Running FASTQC with Slurm
date: 2025-08-23-03:19:53Z
type: docs 
weight: 870
menu: 
    bioinfo-intro:
        parent: Running Jobs
---

Slurm is a resource manager. You can run your codes as Slurm scripts.

Sample Slurm script: `fastqc_slurm_submit.sh`

```bash
#!/bin/bash

#SBATCH -A hpc_training                    # account name (--account)
#SBATCH -p standard                        # partition/queue (--partition)
#SBATCH --nodes=1                          # number of nodes
#SBATCH --ntasks=1                         # 1 task – how many copies of code to run
#SBATCH --cpus-per-task=1                  # total cores per task – for multithreaded code
##SBATCH --mem=3200                        # total memory (Mb) *Note ##comment
#SBATCH -t 00:20:00                        # time limit: 20 min
#SBATCH -J fastqc-test                     # job name
#SBATCH -o fastqc-test-%A.out              # output file
#SBATCH -e fastqc-test-%A.err              # error file
#SBATCH --mail-user=dtriant@virginia.edu   # where to send email alerts
#SBATCH --mail-type=ALL                    # receive email when starts/stops/fails

__module purge__   # good practice to purge all modules
__module load__  fastqc/0.12.1

cd /project/rivanna-training/bioinformatics-hpc

mkdir fastqc-out-slurm

__fastqc__  -t 4 -o fastqc-out-slurm  ecoli-fastq/SRR258*fastq  # command for running software
```
> <small>Mail option output: The mail option notifies you when the job starts and completes. </small>

Job started:

 `Slurm Job_id=4972539 Name=busco-test Began, Queued time 00:00:46`

 Job ended:

{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_38.png width=75% height=75% >}}

### Operational Terminal Commands

To submit your Slurm script from the terminal, use the following command:

```bash
$ sbatch fastqc_slurm_submit.sh
```

<span style="color:#002060">Submitted batch job 4938712</span>


For job monitoring, use the following command:

```bash
$ squeue -u user_id  # checks job status
```

`PD` - pending

`R` - running

`CG` - exiting

The Slurm script method should have the same output files as running the job interactively. 



