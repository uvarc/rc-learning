---
title: PacBio - SMRTLink
date: 2025-08-23-03:19:53Z
type: docs 
weight: 1400
menu: 
    bioinfo-intro:
        parent: Sequence Processing & Alignment
---

### Long-Read Sequencing: PacBio and SMRT Link

A new **PacBio Revio** sequencing machine is available in the **School of Medicine Genomics Core**.  

To run SMRTLink on Rivanna:

```bash
# check which versions are available and load the module
$ module spider SMRT
$ module load smrtlink/25.2.0
```

Lots of tools are available for sequence analysis and data manipulation.

For example, `bam2fasta` converts `BAM` to `FASTA`, and `bam2fastq` converts `BAM` to `FASTQ`.


{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_67.png width=90% height=90% caption="PacBio Revio sequencing instrument interface" >}}

[SMRT Tools Reference Guide](https://docslib.org/doc/175362/smrt%C2%AE-tools-reference-guide-v8-0)

