---
title: Snakemake
date: 2026-03-25T19:08:46Z
type: docs 
weight: 900
menu: 
    bioinfo-reproducibility:
---

**Snakemake** is a workflow management system designed for scientific pipelines

https://snakemake.github.io/

- Created by Johannes Köster, first released in 2012

- Based on UNIX make -  originally created in 1976 but still standard use

- Python based - “ _snake-make_ ”

- Free and open source, available on Mac, Windows, Unix


https://snakemake.readthedocs.io/en/stable/


https://github.com/snakemake


`Make` is a command-line interface software tool that performs actions ordered by configured dependencies as defined in a configuration file called a makefile. It is commonly used for build automation to build executable code from source code. 


### Snakemake Format

- Similar to writing shell scripts but snake files contains sets of rules

- Format is based on Python structure

- Snakemake reads from snakefile that defines the rules

- Snakefile rules have a target output

- Snakemake uses pattern matching to follow the inputs, outputs and commands contained in rules to reach final target output


### Snakemake Core Idea

Instead of defining steps, you define **rules that produce files**
rule align:

input:
	"reads.fastq"

output:

	"aligned.bam"

shell:

	"bwa mem ref.fa {input} > {output}"

Snakemake builds a **directed acyclic graph (DAG)**  automatically.

Fastq → Cutadapt → BWA → Sorted BAM → Freebayes → VCF
