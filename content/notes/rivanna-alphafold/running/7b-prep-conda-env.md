---
title: Building a conda Environment
date: 2025-05-20T00:23:54Z
type: docs 
weight: 620
menu: 
    rivanna-alphafold:
        parent: Running AlphaFold On Rivanna
---

A `conda` enviornment is needed for any post-processing scripts and visualizations of AlphaFold, including running `alphapickle` and generating sequence coverage plots. 

```bash
module load anaconda
conda create --prefix /scratch/$USER/alphafold/conda/env python=3.8
source activate /scratch/$USER/alphafold/conda/env
conda install biopython matplotlib pandas
source deactivate 
```

You can call this  `conda` environment inside a SLURM script:

```bash
conda run -p "/scratch/$USER/alphafold/conda/env" <command to run in env> 
```