---
title: Modify Visualization in VMD
date: 2025-05-20-00:23:54Z
type: docs 
weight: 2150
menu: 
    rivanna-alphafold:
    parent: Visualizing 3D Structures
---

Once your PDB file is loaded in VMD, you can change the visual style using the Graphical Representations menu.

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_45.png >}}


### Representation settings:

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_47.png >}}

**Drawing Method:** Set to `NewCartoon` to show ribbon structure.
**Coloring Method:** Set to `Beta` to visualize `pLDDT` confidence values (stored in the beta column).

### Result

{{< figure src=/notes/rivanna-alphafold/img/Alphafold_48.png caption="Resulting visualization colored by pLDDT via 'beta' slot." >}}


