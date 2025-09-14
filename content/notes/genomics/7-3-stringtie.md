---
title: StringTie
date: 2025-7-10T00:00:00
type: docs 
weight: 500
menu: 
    genomics:
        parent: Software Demos
---

StringTie aligns bulk RNA-Seq reads to a reference genome and estimates the abundance of a particular transcript or gene. It uses a metric called FPKM values, which stands for Fragments Per Kilobase of transcript per Million mapped reads. You can use the outputs from StringTie to estimate expression values.

## Running StringTie with a Slurm Script

The `stringtie_slurm_submit.sh` script below is a Slurm script that will run StringTie. Slurm is a resource manager that can be used to run your code.

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

# Installing StringTie

If your desired module is not installed, you can submit a request via our ticket system (the wait time can vary depending on how popular the request is). You can also install and run StringTie locally.


```bash
git clone https://github.com/gpertea/stringtie
cd stringtie
make -j4 release
```

Look for a README file in the GithHub repository; the README often has download/installation instructions.

To check if your permissions are set to run StringTie, use the `ls -lh` command:

```bash
ls -lh stringtie
```

{{< figure src="/notes/genomics/img/permissions.png" width=60% height=60% caption="How to read file permission settings from example output" >}}

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

## Running StringTie Example

Note your pathway when running StringTie: `/project/rivanna-training/genomics-hpc/stringtie/stringtie`.

We are going to run Test 7.

```bash
cd tests_3
stringtie --mix -G mix_guides.gff -o mix_reads_guided.out.gtf mix_short.bam mix_long.bam
```
In the command above, `--mix` sets the option to read short and long `.bam` files. The order of listed `.bam` files is important; the short `.bam` should be listed first, and the long `.bam` should be listed second. `-G` indicates that the annotation guide is a `.gff` file. The `.gtf` file is the output file.

For more information on StringTie:
https://ccb.jhu.edu/software/stringtie/