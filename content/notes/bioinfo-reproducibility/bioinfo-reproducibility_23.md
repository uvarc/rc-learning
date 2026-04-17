---
title: Snakemake Exercises on HPC
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1200
menu: 
    bioinfo-reproducibility:
---

## Class data:
- GCF_000005845.2_ASM584v2_genomic.fna - genome assembly
- SRR2584863_1.fastq - fastq sequence file, paired-1
- SRR2584863_2.fastq - fastq sequence file, paired-2
- *.smk - snakemake files
- config_variant.yml - configuration file
- submit_snakemake.sh - sample slurm file


## Run interactively - good for testing
```
ijob -c 1 -A allocation -p interactive –v -t 2:00:00
```

## Modules
```bash
module spider <package>
```
- specifics and version of package available

```bash
module spider snakemake
module load snakemake/9.8.1
module list
snakemake -help
```

## Other modules needed
```bash
module load bwa/0.7.17
module load  cutadapt/4.9
module load snakemake/9.8.1
module load freebayes/1.3.10
module load  samtools/1.21

```

## Running snakemake - genome alignment
Snakefile - file.smk, contains rules for snakemake

```bash

snakemake -c 1 -s align.smk
- --dry-run -np good to test first without producing output
- -n only show steps, don't run, -p print shell commands
- -c number of cores
- -s needed if using a named snakefile (if just called "snakefile",  don't need the –s flag)

snakemake --dag| dot -Tpng > dag_align.png

```

## Running snakemake - variant detection
Snakefile - file.smk, contains rules for snakemake

```bash
snakemake -c 1 -s variant-call.smk
- --dry-run
- -c number of cores
- -s needed if using a named snakefile (if just called "snakefile", don't need)

snakemake --dag -s variant-call.smk | dot -Tpng > dag_variant.png

```

