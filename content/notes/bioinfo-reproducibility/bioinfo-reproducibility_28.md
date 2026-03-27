---
title: Running Snakemake - Variant Detection
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1450
menu: 
    bioinfo-reproducibility:
---

Snakefile - file.smk, contains rules for snakemake

```bash
$ snakemake -c 1 -s variant-call.smk

--dry-run

-c number of cores

-s needed if using a named snakefile (if just called "snakefile", don't need)

$ snakemake --dag -s variant-call.smk | dot -Tpng \ > dag_variant.png
```
