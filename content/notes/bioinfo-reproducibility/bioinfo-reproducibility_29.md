---
title: Snakemake Examples on HPC
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1500
menu: 
    bioinfo-reproducibility:
---

- Not recommended to hard-code files within snake file
- Can organize sample names, file paths, and software parameters in a YAML configuration file
- YAML - serialization language that transforms data into a format that can be shared between systems
- With snakemake, configuration file is a reference for the workflow


## Running Snakemake with Config File
Snakefile - file.smk, contains rules for snakemake

```bash
snakemake -c 1 -s variant-yml.smk --configfile config_variant.yml
```
- --configfile – directing snakemake to a config file
-c number of cores
-s needed if using a named snakefile


##  Reproducible Environments


### Snakemake supports reproducible environments

Example with Conda:

rule fastqc:
     input: "reads.fastq"
     output: "qc.html"
     conda:        ”~/.conda/envs/fastqc_env” #path to conda environment

     shell:        "fastqc {input}"

Benefits: Easy dependency management, portable workflows

Can also create a environment.yml file, list conda envs and what to install


## Snakemake with Conda Environment
```bash
module load miniforge
conda create
conda activate
snakemake command
screen/tmux
```

Keeps session running when disconnected

Can create different conda environment for different rules


## Smakemake and containers
rule align:
     container: "docker://biocontainers/bwa"

**Advantages:**
- identical software environments
- portable across HPC systems
- easier collaboration