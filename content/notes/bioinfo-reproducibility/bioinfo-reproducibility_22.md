---
title: Example Snakefile
date: 2026-03-25T19:08:46Z
type: docs 
weight: 1150
menu: 
    bioinfo-reproducibility:
---

```no-highlight

__rule__  all:    input:        "variants/sample1.vcf”

__rule__  trim:

input:

”reads/sample1.fastq”

output:

”trimmed_reads/sample1-trimmed.fastq”

shell:

cutadapt -A TCCGGGTS -o {output} {input}

__rule__  align:    input:        "trimmed_reads/sample1-trimmed.fastq"    output:        "bam/sample1.bam"    threads: 1    shell:        "bwa mem -t {threads} ref.fa {input} | samtools view -Sb - > {output}”

__rule__  call_variants:    input:        "bam/sample1.bam"    output:        "variants/sample1.vcf"    shell:        "freebayes -f ref.fa {input} > {output}”

<span style="color:#0070c0">Snakemake</span>  <span style="color:#0070c0"> takes first rule as the target </span>

<span style="color:#0070c0">then constructs graph of dependencies</span>

<span style="color:#0070c0">{wildcards} serve as placeholders within rules to operate</span>

<span style="color:#0070c0">on multiple files via pattern matching</span>

```

Snakemake builds the entire pipeline graph automatically.


