---
title: What is Nextflow?
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2000
menu: 
    bioinfo-reproducibility:
---


Nextflow is a workflow management system that helps automate and organize multi-step computational pipelines

At a high level, it connects software steps together, manages how data moves between them,
and handles execution across local machines, HPC schedulers like SLURM, or cloud platforms

## Nextflow pipelines
Key concepts:
- Processes, workflows, and parameters
In general, we are going to:
- Create processes to execute desired commands
- Specify parameters to represent workflow settings
- Define a workflow to execute processes in a specific order
Key files:
- main.nf and nextflow.config


