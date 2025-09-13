---
title: SMRT Link 
date: 2025-7-10T00:00:00
type: docs 
weight: 550
menu: 
    genomics:
        parent: Software Demos
---


SMRTLink is a software suite used to analyze data from PacBio sequencers, such as the New Revio sequencing machine in SOM genomics core.

To show what versions of SMRT Link are available, use the `module spider` command:

```bash
module spider SMRT
```

To load a specific version of SMRT Link, such as 25.2.0, use `module load`:

```bash
module load smrtlink/25.2.0
```

The SMRT Link software suite includes *lots* of tools available for sequence analysis and manipulation, such as `bam2fasta`, `bam2fastq`, `blasr`, `pbmm`, `pbalign`, and `isoseq3`.

{{< figure src="/notes/genomics/img/smrtlink.png" width=60% height=60% >}}

For more info on SMRT Link:
https://docslib.org/doc/175362/smrt%C2%AE-tools-reference-guide-v8-0
