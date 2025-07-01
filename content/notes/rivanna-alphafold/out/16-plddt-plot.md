---
title: pLDDT Plot
date: 2025-05-20-00:23:54Z
type: docs 
weight: 1750
menu: 
    rivanna-alphafold:
        parent: Outputs
---

**`ranked_0_pLDDT.png`**

This output plot shows the **per-residue confidence scores** (pLDDT) for the top-ranked AlphaFold model (`ranked_0.pdb`).

For example, CB1 is a 7-transmembrane (7TM) domain protein with less-structured tails on both termini which is reflected in the plot.

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_29.png width=80% height=80% >}}

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_30.png width=30% height=30% >}}

### Key Details

CB1 is a G protein-coupled receptor (GPCR) with a 7-transmembrane domain structure. The N-terminus (5′) is extracellular and the C-terminus (3′) is intracellular. AlphaFold predicts loops between each transmembrane alpha-helix — one of which is noticeably longer and more disordered, as reflected by lower pLDDT values.

Most of the termini are unstructured, which shows up as lower-confidence (yellow/orange) regions in the plot.

<small>Source: Arnold, M. J. (2021) AlphaPickle doi.org/10.5281/zenodo.5708709 </small>