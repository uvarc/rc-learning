---
title: Prep - build a working directory
date: 2025-05-20-00:23:54Z
type: docs 
weight: 4400
menu: 
    rivanna-alphafold:
---


* Navigate to your chosen directory (e.g., cd /scratch/$USER/alphafold)
* Make a directory to hold fasta files (mkdir fasta)
* Make an output directory to hold alphafold outputs (mkdir outdir)
* Make a directory to hold conda environment (mkdir conda)
* Alphafold doesn’t output QC metrics in plot or human-readable formats
* We can access this info with an add-on, alphapickle, which we must install from github:
    * git clone [https://github.com/mattarnoldbio/alphapickle.git](https://github.com/mattarnoldbio/alphapickle.git)
* I have modified a python script to generate a sequence coverage plot, put this in your alphafold directory as well (sequence_coverage_plot.py)
---


Alphapickle is a python package from github, makes QC plots and human-readable QC metric files (csv/txt) from the pickle file output of alphafold
Sequence_coverage_plot_fxn.py is a script I modified from https://raw.githubusercontent.com/jasperzuallaert/VIBFold/main/visualize_alphafold_results.py to generate sequence coverage plots by deriving info from the features.pkl file
Outdir is just an empty directory in which the output files will go 



