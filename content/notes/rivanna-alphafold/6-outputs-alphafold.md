---
title: Outputs
date: 2025-05-20-00:23:54Z
type: docs 
weight: 550
menu: 
    rivanna-alphafold:
        parent: AlphaFold
---

When AlphaFold completes a prediction run, it generates the following output files and directories:

- **`msas/` directory**: Contains the multiple sequence alignments used as input for the model.
- **`features.pkl`**: A machine-readable pickle file containing input features extracted from the MSA and templates.
- **`ranking_debug.json`**: Stores the ranking information used to order the five model outputs.
- **`relax_metrics.json`**: Contains metrics from the AMBER relaxation step.
- **`timings.json`**: Records runtime information for different stages of the prediction pipeline.

Each of the five models produces:
- A **ranked PDB structure**
- A **relaxed PDB structure**
- An **unrelaxed PDB structure**
- A corresponding **`.pkl` file** containing model data