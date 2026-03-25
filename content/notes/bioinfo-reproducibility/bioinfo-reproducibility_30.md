---
title: Running Snakemake with Config File
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1550
menu: 
    bioinfo-reproducibility:
---


Snakefile - file.smk, contains rules for snakemake

```bash
$  snakemake -c 1 -s variant-yml.smk --configfile config_variant.yml
```

--configfile – directing snakemake to a config file

-c number of cores

-s needed if using a named snakefile

