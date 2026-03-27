---
title: Smakemake and containers
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1750
menu: 
    bioinfo-reproducibility:
---

## Snakemake also supports containers:

rule align:    container:        "docker://biocontainers/bwa"

Advantages:

identical software environments

portable across HPC systems

easier collaboration

