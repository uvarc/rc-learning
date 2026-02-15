---
title: Running Bowtie2 - Slurm Script
date: 2025-08-23T03:19:53Z
date: 2025-08-23-03:19:53Z
type: docs 
weight: 1250
menu: 
    bioinfo-intro:
        parent: Sequence Processing & Alignment
---

Sample slurm script: `fastqc_slurm_submit.sh`

The script loads the Bowtie2 module, builds a genome index, and performs read alignment on paired-end FASTQ files.

```bash
#!/bin/bash
#SBATCH -A hpc_training              # account name
#SBATCH -p standard                  # partition/queue
#SBATCH --nodes=1                    # number of nodes
#SBATCH --ntasks=1                   # 1 task
#SBATCH --cpus-per-task=1            # total cores per task
#SBATCH -t 00:20:00                  # time limit: 20 min
#SBATCH -J bowtie2-test             # job name
#SBATCH -o bowtie2-test-%A.out       # output file
#SBATCH -e cutadap-test-%A.err       # error file

module purge  # good practice to purge all modules

module load bowtie2/2.5.4

cd /project/rivanna-training/bioinformatics-hpc

__bowtie2-build __ lambda_virus.fa lambda_virus

__bowtie2__  -x lamda_virus -1 lamda-reads_1.fastq -2 lamda-reads_2.fastq -S lamda.sam
```

