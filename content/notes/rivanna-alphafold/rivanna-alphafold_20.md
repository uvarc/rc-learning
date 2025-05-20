---
title: Working Directory Contents
date: 2025-05-20-00:23:54Z
type: docs 
weight: 1050
menu: 
    rivanna-alphafold:
---


{{< figure src=/notes/rivanna-alphafold/img/Alphafold_21.png >}}


.sif = singularity container file
Slurm scripts for various use cases
Fasta files for both monomer and multimer versions in subdirectory /fasta
Conda environment with python packages necessary for running alphapickle & sequence_coverage_plot.py
Alphapickle is a package from github, makes QC plots and human-readable QC metric files (csv/txt) from the pickle file output of alphafold

Sequence_coverage_plot_fxn.py is a script I modified to generate sequence coverage plots by deriving info from the features.pkl file (from https://raw.githubusercontent.com/jasperzuallaert/VIBFold/main/visualize_alphafold_results.py )

Outdir is just a directory in which the output files will go  - contains cb1 and ubb_multimer output files from previous runs of alphafold, since they take too long to run today


