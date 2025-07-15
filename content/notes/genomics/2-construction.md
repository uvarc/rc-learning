---
title: Assembly Construction
date: 2025-7-10T00:00:00
type: docs 
weight: 100
menu: 
    genomics:
---

The construction of a sequence begins with __short-insert sequence reads__. These short reads are then combined to form longer, contiguous sequences called __contigs__. Contigs are then combined to form even larger __scaffolds__, though there often gaps between the contigs where the sequence is unknown. __Long-read sequencing__ technologies can help order contigs in a sequence, as well as estimate the size of gaps (represented by the string of Ns).

{{< figure src="/notes/genomics/img/construction.png" width=60% height=60% >}}

## Assembly Construction - Long Reads

The assembly construction for long reads is still hierarchical, but proceeds in two rounds. The first round of assembly involves the selection of __seed reads__, or the longest reads in the dataset (wiht a user-defined length cutoff). In the second round, all shorter reads are aligned to the seed reads, in order to generate consensus sequences with high accuracy. We refer to these as __pre-assembled reads__, but they can also be thought of as “error corrected” reads. During the pre-assembly process, seed reads may be split or trimmed at regions of low read coverage (with a user-defined _min cov_ for FALCON sense option). The performance of the pre-assembly process is captured in the pre-assembly stats file.

{{< figure src="/notes/genomics/img/constructionlong.png" width=60% height=60% >}}

## Repetitive Regions

Repetitive regions of DNA are often challenging during genome sequencing and assembly, as it is difficult to figure out where a section belongs in the overall sequence. Over 50% of mammalian genomes are repetitive, and large plant genomes tend to be even worse. For example, the _Arabidopsis thaliana_ plant has a relatively small genome, yet 10% of its genome is made up of repetitive regions.

There are different categories of repetitive regions:
* SINEs - Short Interspersed Nuclear Elements
* LINES - Long Interspersed Nuclear Elements
* LTR - Long Terminal Repeats, retrotransposons
* Segmental duplications
* Low-complexity - microsatellites or homopolymers
