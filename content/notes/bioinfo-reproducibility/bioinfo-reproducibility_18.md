---
title: Snakemake Format
date: 2026-03-25T19:08:46Z
type: docs 
weight: 950
menu: 
    bioinfo-reproducibility:
---

Similar to writing shell scripts but snake files contains sets of rules

Format is based on Python structure

Snakemake reads from snakefile that defines the rules

Snakefile rules have a target output

Snakemake uses pattern matching to follow the inputs, outputs and commands contained in rules to reach final target output

