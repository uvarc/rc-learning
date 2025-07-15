---
title: Assembly Construction
date: 2025-7-10T00:00:00
type: docs 
weight: 100
menu: 
    genomics:
---




short-insert

sequence

reads

Long read sequencing

Scaffold - sequence of contigs, separated by gaps - Ns are predicted gap size

>GTAGTATTCTAGAAAATGTTTTAAATATAGATAGTTGTTTTANATCTGTTAGTTGTCAGATGCTTACTGAATAGTTGGAANNNNNNNNNNNNNNNNNNNNNNNNTGTGAGGTTTTTAGCTCATGAAAGTTTATGATTATTGCACCCCTACTCACAACGAATCCCTATTCTTATCTTTTNNNNNNNNNNNNNNCATGTCACTGGTTTATTTATTTTTTGTGGCTGCAGAAGTCCTTTGTCGCTGTTAATTTTTTGGGAGTTCTCCTGTCGTATATTAAAGCTTTCTTTCTTTTCAGTTTTAAATTTATTTGAACCTTACTATCTTTCTAACAATAAATTGTGGAATTATCAACGAAAACATAGGNNNNNNNNNNNGTCCTTTATACGAAAGCTATATAGTGTTAGGCTTTTCTTTTTTTTTNNNNNNNNGGTGATGTTGTTAATGGTGCCCTTTTCTGGTAATCTTACTAAATCAGTTTGCTTGTTACTTGTATAGTTGT

---

Long-read scaffolding

Assembly Construction - long reads

Still hierarchical but proceeds in two rounds

1. Seed read selection – longest reads in dataset

2. Shorter reads aligned to seed reads for consensus

![](img/genomics_12.png)

![](img/genomics_13.png)

---

The hierarchical genome assembly process proceeds in two rounds. The first round of assembly involves the selection of seed reads, or the longest reads in the dataset (user-defined length cutoff). All shorter reads are aligned to the seed reads, in order to generate consensus sequences with high accuracy. We refer to these as pre-assembled reads but they can also be thought of as “error corrected” reads. During the pre-assembly process, seed reads may be split or trimmed at regions of low read coverage (user-defined min_cov for falcon_sense_option). The performance of the pre-assembly process is captured in the pre-assembly stats file.

Repetitive Regions



* Over 50% of mammalian genomes are repetitive
  * Large plant genomes tend to be even worse
  * _Arabidopsis_  - 10% composition
  * SINEs - Short Interspersed Nuclear Elements
  * LINES - Long Interspersed Nuclear Elements
  * LTR - Long Terminal Repeats, retrotransposons
  * Segmental duplications
  * Low-complexity - Microsatellites or homopolymers


![](img/genomics_14.png)

---

mariner TE found in Drosophila – mosaic eye color
