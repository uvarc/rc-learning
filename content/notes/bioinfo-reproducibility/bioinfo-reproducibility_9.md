---
title: Aspects of Reproducibility
date: 2026-03-25T19:08:46Z
type: docs 
weight: 500
menu: 
    bioinfo-reproducibility:
---

-  Version control

-  Environment management

-  Data storage

-  Containers

-  Tool/software maintenance


## Version Control

### GitHub Click the following link to visit the GitHub site: https://github.com

- Track and manage changes to your code & files

- Store and label changes at every step

- Small or large projects

- Collaborate on projects and minimize conflicting edits

- Works on multiple platforms (MacOS, Windows, Linux)

- Website for github, cutadapt repository



## Envoronment Management

### Conda/Mamba environments

- Isolated spaces for each project with specific tool versions
- Manage Python versions and dependencies
- Install packages and software directly into environment
- Stable and reproducible place to run code and applications
- Not limited to Python, can run bash, Rscript
- YAML configuration file to create or export and transfer an environment


## Storing Results

### Public repositories for sequence data - required for most journals
- Click the following link for the NCBI database: https://www.ncbi.nlm.nih.gov
- Click the following link for the Ensembl database: https://www.ensembl.org/index.html
- Always document and archive changes, especially if unpublished:
   - genome assembly versions
   - sequence data: SNPs, isoforms

## Containers

- Containers are portable environments that run across different computing environments
- They contain packages, software and dependencies that remain isolated from host infrastructure
- Standalone unit of software and can produce same results on different machine or server