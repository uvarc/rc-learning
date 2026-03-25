---
title: Bioinformatic Pipelines
date: 2026-03-25T19:08:46Z
type: docs 
weight: 750
menu: 
    bioinfo-reproducibility:
---

## Typical bioinformatics workflows involve many steps:

* FASTQ → QC → Alignment → Sorting → Variant Calling → Annotation
  * - FASTQ files need quality check and trimming
  * Cutadapt
  * BWA
  * Samtools
  * Freebayes
  * VCFtools
* Create pipeline to string software together for “final” output

