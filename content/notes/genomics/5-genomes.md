---
title: Reference Genomes
date: 2025-07-10T00:00:00Z
type: docs 
weight: 250
menu: 
    genomics:
---

## Reference Genome vs Conventional Genome

Assembling a conventional genome typically makes use of a single sequencing technology. There tends to be a specific goal, such as identifying genetic variations within a population. The resulting sequence has gaps, but the overall cost is lower. On the other hand, a __reference genome__ makes use of multiple sequencing technologies to assemble and results in a complete representation of a genome. There are minimal sequencing gaps, and the overall cost is higher. Many types of genomic analyses such as variant calling, gene expression, and identifying regulatory elements require a strong assembly of a reference genome.

## Databases

There are a wide variety of public, online databases that store reference genomes:  
* https://www.ncbi.nlm.nih.gov
* https://www.ebi.ac.uk/interpro/entry/pfam/#table
* https://www.ensembl.org/index.html
* https://data.faang.org/home
* https://www.ebi.ac.uk
* https://rgd.mcw.edu

## Pangenomes

As sequencing technology advances, the shift in the field is moving from single genome assembly to __pangenomes__ (collections of all genes and sequences found within a species or broader group). Pangenomes are constructed from many resources, such as the reference sequence, variant sequences, raw reads, and haplotype reference panels.

{{< figure src="/notes/genomics/img/pangenome.png" width=70% height=70% caption="Examples of large-scale pangenome projects" >}}

In the example research paper below, researchers constructed a pangenome for soybeans. The researchers performed 26 new genome assemblies and incorporated 3 previously reported assemblies. Through their pangenome, the researchers were able to search for variants that would have been undetectable in a single reference genome.

{{< figure src="/notes/genomics/img/soybeans.png" width=70% height=70% caption="https://doi.org/10.1016/j.cell.2020.05.023" >}}

Overall, genome projects vary greatly in difficulty, reflecting the diverse DNA structures of different organisms. Some organisms are easier to sequence and assemble, such as the Fugu with its relatively small genome, or the Anopheles mosquito with simpler chromosomes. On the other hand, some organisms such as the axolotl have extremely large genomes, while organisms like platypuses have unique chromosomal structures.

{{< figure src="/notes/genomics/img/projects.png" width=70% height=70% >}}