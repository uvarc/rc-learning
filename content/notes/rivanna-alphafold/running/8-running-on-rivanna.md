---
title: Running AlphaFold On Rivanna
date: 2025-05-20-00:23:54Z
type: docs 
weight: 600
menu: 
    rivanna-alphafold:
        name: Running AlphaFold On Rivanna
---

**Overall steps:**

1. Prep the Working Environment 
   - Install `alphapickle` and download the script for sequence coverage plot.

2. Get the Input 
   - Get a `FASTA` file and put it on Rivanna via your preferred method (Globus, `scp`, etc). 

3. Slurm Script
   - Create a SLURM script based on UVA RC AlphaFold documentation.
   - Include post-processing scripts at the end of the SLURM job (e.g., `alphapickle`) for sequence coverage plots and QC visualization.
   - This creates an **all-in-one** script that runs AlphaFold and visualizations in a single job.


4. Submit Job 
   - Submit your slurm script using the command `sbatch alphafold.slurm`.

5. Visualize
   - Visualize QC plots and 3D structures in OOD Desktop. 