---
title: Look at a Trim Rule
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2300
menu: 
    bioinfo-reproducibility:
---

Let's look at our snakemake "trim" rule from earlier:

__rule__  trim:

input:

”reads/sample1.fastq”

output:

”trimmed_reads/sample1-trimmed.fastq”

shell:

cutadapt -A TCCGGGTS -o {output} {input}

{{< figure src=/notes/bioinfo-reproducibility/img/Triant-Bobar_Reproducibility_41.png >}}


Here we specified our inputs/outputs and our shell command.

