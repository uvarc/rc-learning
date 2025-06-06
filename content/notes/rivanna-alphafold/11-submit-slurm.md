---
title: Submit Slurm Job
date: 2025-05-20-00:23:54Z
type: docs 
weight: 1100
menu: 
    rivanna-alphafold:
---

To begin the AlphaFold run, use the `sbatch` command to submit the SLURM script designed for the reduced monomer database configuration:

```bash
sbatch monomer_reduceddbs_alphafold.slurm
```

While the job runs in the background, you can continue following along using an example protein. Pre-computed results for this example are located in the outdir subdirectory.
