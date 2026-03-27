---
title: A More Common Approach
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2450
menu: 
    bioinfo-reproducibility:
---

A better approach is to pass the file into the process with Channel.fromPath() and use input: path reads. The "input:" declares an input variable, not a literal source file location. And we use this variable "reads" our shell command and here $reads means: the local process input variable and use the actual input file that was provided to Nextflow for this task via our workflow. We can also use the reads variable to other things like dynamically name files or any

__process__  CUTADAPT {    publishDir "results/" , mode: "copy"

<span style="color:#000000">input:</span>  <span style="color:#000000">    path </span>  <span style="color:#000000">reads_var</span>

output:    path 'trimmed.fastq'    script:    """    cutadapt -a AACCGGTT -o trimmed.fastq $reads_var    """}

workflow {

CUTADAPT(Channel.fromPath('~/sample1.fastq', checkIfExists: true))

}



