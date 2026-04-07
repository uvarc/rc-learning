---
title: Recommended Pipeline Directory Structure
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1050
menu: 
    bioinfo-reproducibility:
---


## Benefits:
- separates  **workflow logic from data**
- easier debugging
- easier collaboration


## Common practice:
- config/ → parameters and sample tables
- envs/ → reproducible environments
- rules/ → modular workflow steps
- results/ → generated outputs

A clean directory structure makes pipelines easier to maintain and reproduce.


## Snakefile Breakdown
- Fastq files that need trimming - input:  sample.fastq
- Cutadapt - output: sample-trimmed.fastq
- BWA - align trimmed fastq to assembly output: sample-aligned.sam
- Samtools sorting, indexing - output: sample-sorted.bam
- Freebayes variant calling - output: sample-variants.vcf


## Example snakefile
rule all:

     input:
     
	variants/sample1.vcf

**Snakemake take first rule as target then constructs graph of dependencies**

rule trim:

     input:
     
	”reads/sample1.fastq”
	output:
	
	 ”trimmed_reads/sample1-trimmed.fastq”
	shell:
	
	  cutadapt -A TCCGGGTS -o {output} {input} 

rule align:

     input:
     
	"trimmed_reads/sample1-trimmed.fastq"
     output:
     
        "bam/sample1.bam"
	
     threads: 1
     
     shell:
     
        bwa mem -t {threads} ref.fa {input} | samtools view -Sb -> {output}"
	

**wildcards** serve as placeholders within rules to operate on multiple files via pattern matching


