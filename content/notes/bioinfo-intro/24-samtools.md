---
title: Samtools
date: 2025-08-23T03:19:53Z
date: 2025-08-23-03:19:53Z
type: docs 
weight: 1350
menu: 
    bioinfo-intro: 
        parent: Sequence Processing & Alignment
---

After aligning reads with Bowtie2, the resulting output file is in **SAM format**.  
Because SAM files are large and not indexed, we use **Samtools** to convert them into the compressed **BAM** format and sort them for downstream analysis.

**Samtools** is a suite of programs for analyzing sequence data.

Running Samtools interactively:

```bash
$ module spider samtools

$ module load samtools/1.21

# look at difference in file sizes
$ samtools view -S -b lamda.sam > lamda.bam

$ ls -lh lamda.*

$ samtools sort lamda.bam > lamda-sorted.bam # puts in same order as fastq files

```

