---
title: Using Environments
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1650
menu: 
    bioinfo-reproducibility:
---


├── Snakefile├── config/│   └── config.yml├── envs/│   └── bwa.yml├── rules/│   ├── alignment.smk│   ├── qc.smk│   └── variant_calling.smk├── scripts/│   └── custom_processing.py├── data/│   └── raw/├── results/│   ├── bam/│   ├── qc/│   └── variants/└── logs/

Can also create a environment.yml file, list conda envs and what to install

__name__ : bwa.yml

__channels:__

- conda-forge

- bioconda

__dependencies__ :

-bwa= <span style="color:#000000">0.7.17</span>

---

.yml file can indicate how to make conda environment and what packages and dependencies you need

