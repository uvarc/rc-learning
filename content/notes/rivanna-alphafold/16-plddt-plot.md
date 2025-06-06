---
title: pLDDT Plot
date: 2025-05-20-00:23:54Z
type: docs 
weight: 1750
menu: 
    rivanna-alphafold:
    parent: Outputs
---

###  `ranked_0_pLDDT.png`

This plot shows the **per-residue confidence scores** (pLDDT) for the top-ranked AlphaFold model (`ranked_0.pdb`).

CB1 is a **7-transmembrane (7TM) domain protein** with less-structured tails on both termini which is reflected in the plot.

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_29.png >}}

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_30.png >}}

### Key Details

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_28.png caption="Schematic of a GPCR (7-transmembrane domain) topology.">}}

CB1 is a G protein-coupled receptor (GPCR) with a 7-transmembrane domain structure. The **N-terminus (5′)** is extracellular and the **C-terminus (3′)** is intracellular. AlphaFold predicts loops between each transmembrane alpha-helix — one of which is noticeably longer and more disordered, as reflected by lower pLDDT values.

While there are a couple of short helices near the C-terminal tail, most of the termini are unstructured, which also shows up as lower-confidence (yellow/orange) regions in the plot.

>Source: Arnold, M. J. (2021) AlphaPickle doi.org/10.5281/zenodo.5708709