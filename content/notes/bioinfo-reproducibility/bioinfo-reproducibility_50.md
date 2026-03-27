---
title: Parameter Options for Input Files
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2550
menu: 
    bioinfo-reproducibility:
---
As with many things with Nextflow, we have multple different ways we can accomplish this. We'll talk about nextflow.config shortly.

Add a parameter for '--reads' in your 'nextflow run' command

Add a params.reads at the top of your main.nf file

Add a params.reads to a nextflow.config file

Works for one file ('reads/sample1.fastq') or many ('reads/*.fastq')



