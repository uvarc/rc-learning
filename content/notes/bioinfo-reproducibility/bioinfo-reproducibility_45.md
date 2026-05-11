---
title: Look at a Trim Rule
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2300
menu: 
    bioinfo-reproducibility:
---

## Let's look at our snakemake "trim" rule from earlier:

Here we specified our inputs/outputs and our shell command.

```python
rule trim:
 input:
 "reads/sample1.fastq"

 output:
 "trimmed_reads/sample1-trimmed.fastq"

 shell:
 cutadapt -A TCCGGGTS -o {output} {input}
```

## What to Update in Nextflow?

In looking at our HELLO process, what do we need to add? We already have a publishDir, an output, and script, so let's update those for cutadapt.

```groovy
process HELLO {
 publishDir "results/" , mode: "copy"
 output:
 path 'hello.txt'

 script:
 """
 echo 'Hello world!' > hello.txt
 """
}
```

## Update for Running Cutadapt
```groovy
process  CUTADAPT {
 publishDir "results/" , mode: "copy"

 output:
 path 'trimmed.fastq'

 script:
 """
 cutadapt -a AACCGGTT -o trimmed.fastq ~/sample1.fastq
 """
}

workflow {

CUTADAPT()
}
```

We can keep 'results' as our publishDir for this example, but we'll need to change our output to trimmed.fastq and we'll change the command for cutadapt with our adapter and our input and output file names. Because Nextflow executes each task in its own work directory, we need to provide the full path. Our workflow just becomes running the CUTADAPT process.

Does this work? Yes, it does. However, note that  we are hard-coding everything and this not really flexible and does not really allow us to scale.


