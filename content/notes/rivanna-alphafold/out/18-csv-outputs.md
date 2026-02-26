---
title: CSV Outputs - alphapickle
date: 2025-05-20T00:23:54Z
type: docs 
weight: 1850
menu: 
    rivanna-alphafold:
        parent: Outputs
---

AlphaPickle generates CSV files that can be used for downstream analysis and plotting.

**`PAE.csv`**

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_32.png width=70% height=70% caption="Example PAE.csv file contents." >}}

`PAE.csv` is an `n_residue` x `n_residue` matrix that is used for generating PAE heatmaps.

**`pLDDT.csv`**

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_33.png width=20% height=20% caption="Example pLDDT.csv file contents." >}}

`pLDDT.csv` is an `n_residue`-long list which is helpful for generating pLDDT plots.

You can use these files to generate your own custom plots! Try Python, R, or your favorite plotting method.




