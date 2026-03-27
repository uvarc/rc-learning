---
title: Running snakemake - genome alignment
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1400
menu: 
    bioinfo-reproducibility:
---

Snakefile - file.smk, contains rules for snakemake

```bash

$ snakemake -c 1 -s align.smk

--dry-run -np good to test first without producing output

-n only show steps, don't run, -p print shell commands

-c number of cores

-s needed if using a named snakefile (if just called "snakefile",  don't need the –s flag)

$ snakemake --dag| dot -Tpng > dag_align.png
```

