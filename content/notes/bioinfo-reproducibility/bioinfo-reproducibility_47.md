---
title: Update for Running Cutadapt
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2400
menu: 
    bioinfo-reproducibility:
---


__process__  CUTADAPT {    publishDir "results/" , mode: "copy"

output:    path 'trimmed.fastq'    script:    """    cutadapt -a AACCGGTT -o trimmed.fastq ~/sample1.fastq    """}

workflow {

CUTADAPT()

}


We can keep 'results' as our publishDir for this example, but we'll need to change our output to trimmed.fastq and we'll change the command for cutadapt with our adapter and our input and output file names. Because Nextflow executes each task in its own work directory, we need to provide the full path. Our workflow just becomes running the CUTADAPT process.
Does this work? Yes, it does. However, but we are hard-coding everything and this not really flexible and does not really allow us to scale.

