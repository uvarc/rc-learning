---
title: Recommended Pipeline Directory Structure
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1050
menu: 
    bioinfo-reproducibility:
---


Benefits:

separates  __workflow logic from data__

easier debugging

easier collaboration

Common practice:

config/ → parameters and sample tables

envs/ → reproducible environments

rules/ → modular workflow steps

results/ → generated outputs

Example:

bioinformatics_pipeline/├── Snakefile├── config/│   └── config.yml├── envs/│   └── bwa.yml├── rules/│   ├── alignment.smk│   ├── qc.smk│   └── variant_calling.smk├── scripts/│   └── custom_processing.py├── data/│   └── raw/├── results/│   ├── bam/│   ├── qc/│   └── variants/└── logs/

A clean directory structure makes pipelines easier to maintain and reproduce.

---

.yml file can indicate how to make conda environment and what packages and dependencies you need

