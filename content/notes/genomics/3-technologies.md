---
title: Sequencing Technologies
date: 2025-7-10T00:00:00
type: docs 
weight: 150
menu: 
    genomics:
---

There are many sequencing technologies in the field of DNA sequencing. Some of the most popular technologies currently include:

* __Illumina__ - produces short read sequences (< 1kb), commonly used for whole genome sequencing, exomes, microRNA, and single-cell apps
* __PacBio__ - produces long read sequences (~ 25 kb), used in the Revio sequencer at UVA
* __Nanopore__ - produces “ultra-long” sequences (up to 1Mb)
* __Hi-C__ -  a crosslinking technique that captures interactions within genome

## PacBio HiFi Reads

PacBio produces long reads typically around 25kb, with a 99.9% read quality (Q30), which is better than some short-read contigs. PacBio HiFi reads also distinguish repeats rather than spanning full repeats. However, their reads are limited in length and have lower throughput than Nanopore.

{{< figure src="/notes/genomics/img/pacbio.png" width=70% height=70% >}}

### Phased Genome Assemblies

Standard assemblies produce __unphased__ sequences, where the variation from two chromosomes is collapsed into a __pseudo-haplotype__ (a single, mixed sequence). PacBio assemblies allow for the production of __phased__ sequences, where the variation from two chromosomes is preserved in two separate sequences.

{{< figure src="/notes/genomics/img/phased1.jpg" width=50% height=50% >}}

{{< figure src="/notes/genomics/img/phased2.png" width=70% height=70% >}}

## Nanopore Sequencing

{{< figure src="/notes/genomics/img/nanopore.jpg" width=40% height=40% >}}

Nanopore is able to produce “ultra-long” reads up to 4 Mb. Its accuracy is at about 95% read quality, and it is able to span repeat regions. The quality is limited compared to other technologies, and it has taken a while for Nanopore's technology to reach where it is today.

{{< figure src="/notes/genomics/img/nanoporedevice.jpg" width=50% height=50% caption="A sample-to-sequence portable Nanopore device" >}}

{{< figure src="/notes/genomics/img/katerubins.png" width=40% height=40% caption="Kate Rubins using Nanopore on the ISS" >}}

Nanopore uses long reads and coverage to detect repeats.

{{< figure src="/notes/genomics/img/nanoporerepeat.png" width=50% height=50% >}}

## Hi-C

Hi-C is a genomic technique used to capture chromatin information. It involves detecting and analyzing the frequency of contacts between regions of DNA, in order to determine the correct order and orientation of the DNA segments. For example, segments with more contact are likely to be adjacent or closer to each other.

{{< figure src="/notes/genomics/img/hic1.png" width=50% height=50% >}}

{{< figure src="/notes/genomics/img/hic2.png" width=70% height=70% >}}

## References

* Wenger et al. _Nature Biotechnology_ 2019
* Chin et al. _Nat Meth_ 2016
* https://www.pacb.com/blog/ploidy-haplotypes-and-phasing/
* Shafin et al. _Nature Biotechnology_ 2020
* How it Works: Proximo Hi-C Genome Scaffolding https://www.youtube.com/watch?v=-MxEw3IXUWU