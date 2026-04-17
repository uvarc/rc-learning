---
title: Best Practices for HPC
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1800
menu: 
    bioinfo-reproducibility:
---

## Recommendations:

- Use threads and resources properly
- Avoid huge single jobs
- Break workflows into modular rules
- Use conda or containers
- Use --dry-run before submitting large workflows
- Store configuration in YAML files


## Common HPC Pitfalls with Workflow Managers
**Examples:**
- Requesting too many cores per rule
- Forgetting to specify memory
- Submitting thousands of tiny jobs
- Running Snakemake or Nextflow themselves on a login node


## Key Takeaways with Workflow Managers
Snakemake & Nextflow provide:
- Reproducible pipelines
- Automatic dependency tracking
- Scalable HPC execution
- Environment management
- Workflow portability