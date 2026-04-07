---
title: A More Common Approach
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2450
menu: 
    bioinfo-reproducibility:
---

A better approach is to pass the file into the process with Channel.fromPath() and use input: path reads. The "input:" declares an input variable, not a literal source file location. And we use this variable "reads" our shell command and here $reads means: the local process input variable and use the actual input file that was provided to Nextflow for this task via our workflow. We can also use the reads variable to other things like dynamically name files

```bash
process  CUTADAPT {
 publishDir "results/" , mode: "copy"
 input:
 path reads_var

 output:
 path 'trimmed.fastq'

 script:
 """
 cutadapt -a AACCGGTT -o trimmed.fastq $reads_var
 """
}

workflow {
CUTADAPT(Channel.fromPath('~/sample1.fastq', checkIfExists: true))
}
```

## Dynamically Scaling to Many Samples
Now we can start to use the flexibility nextflow provides to name our output files dynamically based on sample name and we also can start to scale upby using the wildcard to grab all the fastq files in our example 'reads' directory. Here nextflow is going to create a new separate process for each of our samples.

```bash
process CUTADAPT {
 publishDir "results/", mode: "copy"

 input:
 path reads_var

 output:
 path "${reads_var.simpleName}_trimmed.fastq"

 script:
 """
 cutadapt -a AACCGGTT -o ${reads_var.simpleName}_trimmed.fastq $reads_var
 """
}

workflow {
 CUTADAPT(Channel.fromPath('*.fastq',checkIfExists: true))
}
```

## Parameter Options for Input Files
- Add a parameter for '--reads' in your 'nextflow run' command
- Add a params.reads at the top of your main.nf file
- Add a params.reads to a nextflow.config file
- Works for one file ('reads/sample1.fastq') or many ('reads/*.fastq')


## Less Hard-coding = More Reproducibility
If we use one of those parameter methods, instead of our workflow having a hard-coded path for our inputs, we can dynamically provide our input file names and clean things up in our workflow even further.

From:
workflow {
CUTADAPT(Channel.fromPath(~/sample1.fastq', checkIfExists: true))
}

To:
workflow {
CUTADAPT(Channel.fromPath(params.reads, checkIfExists: true))
}

