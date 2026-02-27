---
title: PAE Plot
date: 2025-05-20T00:23:54Z
type: docs 
weight: 1800
menu: 
    rivanna-alphafold:
        parent: Outputs
---

**`ranked_0_PAE.png`**

This output plot visualizes the **Predicted Aligned Error (PAE)** for the top-ranked model (`ranked_0.pdb`).

Each color at position (x, y) indicates the expected position error at residue x, when the predicted and true structures are aligned on residue y.

The color scale indicates  _expected distance error_ in angstroms. Purple is 'good' (represents low error), whereas yellow is 'bad' (represents high error).

For example, the CB1 protein is a 7-transmembrane domain GPCR with less-structured tails on both termini, which appear as regions of higher predicted error.

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_31.png width=60% height=60% >}}

<small>Source: Arnold, M. J. (2021) AlphaPickle doi.org/10.5281/zenodo.5708709</small>

[More Information](https://alphafold.ebi.ac.uk/entry/Q9Y223)(see "Predicting aligned error tutorial" section)

