---
title: Dynamically Scaling to Many Samples
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2500
menu: 
    bioinfo-reproducibility:
---

Now we can start to use the flexibility nextflow provides to name our output files dynamically based on sample name and we also can start to scale up by using the wildcard to grab all the fastq files in our example 'reads' directory. Here nextflow is going to create a new separate process for each of our samples.


__process CUTADAPT {__  __ __  __publishDir__  __ "results/", mode: "copy"__  __ input:__  __ path __  __reads_var__  __ output:__  __ path "${__  __reads_var.simpleName__  __}___  __trimmed.fastq__  __"__  __ script:__  __ """__  __ __  __cutadapt__  __ -a AACCGGTT -o ${__  __reads_var.simpleName__  __}___  __trimmed.fastq__  __ $__  __reads_var__  __ """__  __}__  __workflow {__  __ CUTADAPT(__  __Channel.fromPath__  __('*.__  __fastq__  __', __  __checkIfExists__  __: true))__  __}__




