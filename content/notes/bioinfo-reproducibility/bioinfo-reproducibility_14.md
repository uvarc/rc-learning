---
title: Bioinformatic Pipelines
date: 2026-03-25T19:08:46Z
type: docs 
weight: 750
menu: 
    bioinfo-reproducibility:
---

## Typical bioinformatics workflows involve many steps:

### FASTQ → QC → Alignment → Sorting → Variant Calling → Annotation
- FASTQ files need quality check
- Cutadapt for trimming
- BWA - genome alignment
- Samtools - file formatting and conversions
- Freebayes - variant calling
- VCFtools - manipulating files

**Create pipeline to string software together for “final” output**

## Bioinformatic Pipeline Challenges
- Complex dependencies between steps
- Formatting inconsistencies
- Hard to reproduce results - scalability, parameters, version changes
- Difficult to parallelize efficiently
- Manual scripts often fail on HPC

## Bioinformatic Pipelines on HPC
- Which modules were loaded?
- Where are scripts being run?
- Tracking paths - hard-coded in scripts?
- Out/error files - software vs slurm conflicts

**Goal:** Automate and track these workflows
