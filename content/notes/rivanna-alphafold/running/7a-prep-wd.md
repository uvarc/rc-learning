---
title: Building a Working Directory
date: 2025-05-20-00:23:54Z
type: docs 
weight: 610
menu: 
    rivanna-alphafold:
      parent: Running AlphaFold On Rivanna
---

Steps to manually build an AlphaFold working directory:

* Navigate to your chosen directory (e.g., `cd /scratch/$USER/alphafold`)
* Make a directory to hold `fasta` files (`mkdir fasta`)
* Make an output directory to hold AlphaFold outputs (`mkdir outdir`)
* Make a directory to hold `conda` environment (`mkdir conda`)
* AlphaFold doesn’t output QC metrics in plot or human-readable formats. We can access this info with an add-on, `alphapickle`, a python package from GitHub that makes QC plots and human-readable QC metric files (`.csv`/`.txt`) from the pickle file output of AlphaFold. Install `alphapickle` from GitHub:
```bash
  git clone [https://github.com/mattarnoldbio/alphapickle.git](https://github.com/mattarnoldbio/alphapickle.git)
```
* The author has modified a python script to generate a sequence coverage plot. Put `sequence_coverage_plot.py` in your AlphaFold directory.

Other notes:

`Sequence_coverage_plot_fxn.py` is a script modified from https://raw.githubusercontent.com/jasperzuallaert/VIBFold/main/visualize_alphafold_results.py to generate sequence coverage plots by deriving info from the `features.pkl` file.

`Outdir` is an empty directory in which the output files will go.



