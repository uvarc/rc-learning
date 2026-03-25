---
title: Snakemake Core Idea
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1000
menu: 
    bioinfo-reproducibility:
---

Instead of defining  _steps_ , you define  __rules that produce files__ .

rule align:

input:

"reads.fastq"

output:

"aligned.bam"

shell:

"bwa mem ref.fa {input} > {output}"

Snakemake builds a  __directed acyclic graph (DAG)__  automatically.

Fastq → Cutadapt → BWA → Sorted BAM → Freebayes → VCF

