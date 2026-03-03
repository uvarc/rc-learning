---
title: Chemprop Application, Part III
date: 2025-03-11T13:24:11Z
type: docs 
weight: 700
menu: 
    dl-drug-discovery:
        parent: Chemprop Application Case Study
---

#### Screening the ZINC Library with Chemprop

Following the discovery of Halicin, the Chemprop model was scaled up to screen for additional antibiotic candidates. 
The researchers applied the model to a massive chemical space — the **ZINC compound library**, which contains over 107 million molecules.

---

#### Large-Scale In-Silico Screening

The figure below shows the results of this large-scale screen.  
Out of the full set, the model predicted thousands of molecules to have high activity scores.  
These were filtered based on prediction confidence.

{{< figure src="/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_28.png" caption="Panel B: Distribution of prediction scores across 107 million ZINC molecules. Panel C: High-scoring compounds (yellow) selected for follow-up based on their predicted antibiotic activity and structural novelty." >}}

---

#### Experimental Testing of Top Candidates

From this screen, several top candidates were selected for experimental testing.  
Two of the compounds — **ZINC000100032716** and **ZINC000225434673** — showed strong promise in lab experiments.

{{< figure src="/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_29.png" width=40% height=40% caption="Table showing compound structures and their activity against multiple bacterial strains. Lower values (in red) indicate stronger activity." >}}

{{< figure src="/notes/dl-drug-discovery/img/Deep-Learning-Drug-Discovery_30.png" caption="Experimental validation of two ZINC-derived compounds. The red lines show a significant drop in bacterial count (CFU/mL) after exposure, confirming strong antibiotic effect." >}}

---

Chemprop’s in-silico screening helped identify potential new compounds with antibiotic-like properties.