---
title: PDB Structure
date: 2025-05-20T00:23:54Z
type: docs 
weight: 1900
menu: 
    rivanna-alphafold:
        parent: Outputs
---

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_34.png width=40% height=40% caption="Predicted 3D structure of CB1." >}}

The above image shows an example of a 3D structure from a `ranked_0.pdb` file. The structure is colored by `pLDDT`, where dark blue represents high confidence and red represents low confidence.

CB1 is a 7-transmembrane domain (7TM) septa-helical protein with less-structured tails on both termini.
This is reflected in the 3D structure, where disordered regions ("ribbons") are low-confidence and helices are high-confidence.

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_35.png width=40% height=40% caption="Schematic diagram of CB1.">}}

This schematic complements the 3D model by showing the membrane topology of CB1. The arrangement of helices and loops explains the structured and unstructured regions seen in the AlphaFold prediction.

The GPCR N-terminus (5’/start of protein) is extracellular, and the C-terminus (3’/end of protein) is intracellular. There is a loop between each transmembrane alpha-helix; one loop is longer than the others, which is why you see it is more unstructured. This GPCR happens to have a couple short helices in its C terminal structure as well, but mostly the tails are disordered/unstructured.

