---
title: Confidence Metrics
date: 2025-05-20-00:23:54Z
type: docs 
weight: 580
menu: 
    rivanna-alphafold:
        parent: AlphaFold
---

AlphaFold uses two confidence metrics: PAE and pLDDT.

### **PAE: Predicted Aligned Error**

PAE is the expected position error at residue X when predicted and true structures are aligned on residue Y.

<small>[More information](https://alphafold.ebi.ac.uk/entry/Q9Y223)</small>

### **pLDDT: Predicted Local Distance Difference Test**

pLDDT is a per-residue confidence score (0–100).
It is a "superposition-free score that evaluates local distance differences of all atoms in a model, including validation of stereochemical plausibility."

<small>[More information](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3799472/)</small>

