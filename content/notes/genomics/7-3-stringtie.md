---
title: StringTie
date: 2025-7-10T00:00:00
type: docs 
weight: 500
menu: 
    genomics:
        parent: Software Demos
---






## Running StringTie

StringTie aligns bulk RNA-Seq reads to a reference genome and estimates abundance

Based on FPKM values (Fragments Per Kilobase of transcript per Million mapped reads)

Can use outputs to estimate expression values

https://ccb.jhu.edu/software/stringtie/


## Running StringTie – slurm script

The `stringtie_slurm_submit.sh` script below is a Slurm scipt that will run StringTie. Slurm is a resource manager that can be used to run your code.

```bash
#!/bin/bash
#SBATCH -A hpc_training                    # account name (--account)
#SBATCH -p standard                        # partition/queue (--partition)
#SBATCH --nodes=1                          # number of nodes
#SBATCH --ntasks=1                         # 1 task – how many copies of code to run
#SBATCH --cpus-per-task=1                  # total cores per task – for multithreaded code
##SBATCH --mem=3200                        # total memory (Mb) *Note ##comment
#SBATCH -t 00:20:00                        # time limit: 20 min
#SBATCH -J stringtie-test                  # job name
#SBATCH -o stringtie-test-%A.out           # output file
#SBATCH -e stringtie-test-%A.err           # error file
#SBATCH --mail-user=dtriant@virginia.edu   # where to send email alerts
#SBATCH --mail-type=ALL                    # receive email when starts/stops/fails

module purge   # good practice to purge all modules
module load stringtie/2.2.1

cd /project/rivanna-training/genomics-hpc/stringtie/tests_3 # working directory
stringtie  --mix -G mix_guides.gff -o mix_reads_guided.out.gtf mix_short.bam mix_long.bam
# -- mix both short and long reads aligning
# -G reference annotation for guided alignment with short (1st) & long (2nd) read bam alignment files
# generates .gtf file with assembled transcripts and expression levels
```


## Running Jobs

Interactively

Loading modules

Downloading software locally



## Running StringTie

What if the module is not installed?

- Submit a request via ticket system - waiting time, popular request

- Install it yourself and run locally

- Run from your directory but submitted to computing nodes via

slurm script

Installing software “locally”

https://github.com/gpertea/stringtie

```bash
git clone https://github.com/gpertea/stringtie
cd stringtie
make -j4 release
```

Running StringTie

Installing software “locally”

https://github.com/gpertea/stringtie

```bash
git clone https://github.com/gpertea/stringtie
cd stringtie
make -j4 release
```

Look for a README file - often has download/installation instructions

$ ls -lh stringtie

-rwxrwx---+ 1 dat2g rc-staff 14M Jun  3 22:37 stringtie

- file permissions


## Running StringTie

File permissions

$ ls -lh stringtie -rwxrwx---+ 1 dat2g rc-staff 14M Jun  3 22:37 stringtie

- file permissions

-rw-r--r-- 12 linuxize users 12.0K Apr  8 20:51 filename.txt

|[-][-][-]-   [------] [---]

| |  |  | |      |       |

| |  |  | |      |       +-----------> 7. Group

| |  |  | |      +-------------------> 6. Owner

| |  |  | +--------------------------> 5. Alternate Access Method

| |  |  +----------------------------> 4. Others Permissions

| |  +-------------------------------> 3. Group Permissions

| +----------------------------------> 2. Owner Permissions

+------------------------------------> 1. File Type. - regular file, d directory

File not readable

r    File is readable

w   File is  writeable

x   File is  executable

![](img/genomics_71.png)

## Running StringTie

Installing software “locally”

https://github.com/gpertea/stringtie

```bash
git clone https://github.com/gpertea/stringtie
cd stringtie
make -j4 release
```

To download and test sample data:

```bash
./run_test.sh
```

We are going to run Test 7 - Mixed reads with annotation guides:

```bash
stringtie --mix -G mix_guides.gff -o mix_reads_guided.out.gtf mix_short.bam mix_long.bam
```



## Running StringTie

Run `./run_test.sh` to download and test sample data.

__Test 1__: Short reads

```bash
../stringtie -o short_reads.out.gtf short_reads.bam
```

__Test 2__: Short reads and super-reads

```bash
../stringtie -o short_reads_and_superreads.out.gtf short_reads_and_superreads.bam
```

__Test 3__: Short reads with annotation guides

```bash
../stringtie -G mix_guides.gff -o short_guided.out.gtf mix_short.bam
```

__Test 4__: Long reads

```bash
../stringtie -L -o long_reads.out.gtf long_reads.bam
```

__Test 5__: Long reads with annotation guides

```bash
../stringtie -L -G human-chr19_P.gff -o long_reads_guided.out.gtf long_reads.bam
```

__Test 6__: Mixed reads

```bash
../stringtie --mix -o mix_reads.out.gtf mix_short.bam mix_long.bam
```

__Test 7__: Mixed reads with annotation guides

```bash
../stringtie --mix -G mix_guides.gff -o mix_reads_guided.out.gtf mix_short.bam mix_long.bam
```

__Test 8__: Short reads with `-N`

```bash
../stringtie -N -G mix_guides.gff -o mix_short_N_guided.out.gtf mix_short.bam
```

__Test 9__: Short reads with `--nasc`

```bash
../stringtie --nasc -G mix_guides.gff -o mix_short_nasc_guided.out.gtf mix_short.bam
```

## Running StringTie

Note your pathway

/project/rivanna-training/genomics-hpc/stringtie/stringtie

$ cd tests_3

$  __stringtie__  --mix -G mix_guides.gff  -o mix_reads_guided.out.gtf mix_short.bam mix_long.bam

- m - mixed reads short/long

- G - .gff file - annotation guide

- order of bams important: short first, long second

- .gtf output file
