---
title: Snakemake Exercises on HPC
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1200
menu: 
    bioinfo-reproducibility:
---

Class data:

<span style="color:#1a1a1a">/project/</span>  <span style="color:#1a1a1a">hpc_training</span>  <span style="color:#1a1a1a">/reproducibility/</span>  <span style="color:#1a1a1a">snakemake</span>

$ cp  <span style="color:#1a1a1a">/project/</span>  <span style="color:#1a1a1a">hpc_training</span>  <span style="color:#1a1a1a">/reproducibility/</span>  <span style="color:#1a1a1a">snakemake</span>  <span style="color:#1a1a1a"> .</span>

<span style="color:#1a1a1a">   </span>  <span style="color:#1a1a1a">- GCF_000005845.2_ASM584v2_genomic.fna - genome assembly</span>

<span style="color:#1a1a1a">    - SRR2584863_1.fastq - </span>  <span style="color:#1a1a1a">fastq</span>  <span style="color:#1a1a1a"> sequence file, paired-1</span>

<span style="color:#1a1a1a">    - SRR2584863_2.fastq - </span>  <span style="color:#1a1a1a">fastq</span>  <span style="color:#1a1a1a"> sequence file, paired-2</span>

<span style="color:#1a1a1a">    - *.</span>  <span style="color:#1a1a1a">smk</span>  <span style="color:#1a1a1a">  - </span>  <span style="color:#1a1a1a">snakemake</span>  <span style="color:#1a1a1a"> files</span>

<span style="color:#1a1a1a">    - </span>  <span style="color:#1a1a1a">config_variant.yml</span>  <span style="color:#1a1a1a">  - configuration file</span>

<span style="color:#1a1a1a">    - submit_snakemake.sh - sample </span>  <span style="color:#1a1a1a">slurm</span>  <span style="color:#1a1a1a"> file</span>


Yet another markup language- YAML Ain't Markup Language 

