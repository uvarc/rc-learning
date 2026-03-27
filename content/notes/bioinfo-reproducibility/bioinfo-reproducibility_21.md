---
title: Snakefile Breakdown
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1100
menu: 
    bioinfo-reproducibility:
---


Fastq files that need trimming - input:  sample.fastq

Cutadapt - output: sample-trimmed.fastq

BWA - align trimmed fastq to assembly output: sample-aligned.sam

Samtools sorting, indexing - output: sample-sorted.bam

Freebayes variant calling - output: sample-variants.vcf

