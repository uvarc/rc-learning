---
title: Reproducible Environments
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1600
menu: 
    bioinfo-reproducibility:
---

## Snakemake supports reproducible environments.

Example with Conda:

rule fastqc:    input: "reads.fastq"    output: "qc.html"    conda:        ”~/.conda/envs/fastqc_env” #path to conda environment    shell:        "fastqc {input}"

Benefits: Easy dependency management, portable workflows

.yml file can indicate how to make conda environment and what packages and dependencies you need

