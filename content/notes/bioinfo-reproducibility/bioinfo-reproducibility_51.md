---
title: Less Hard-coding = More Reproducibility
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2600
menu: 
    bioinfo-reproducibility:
---

If we use one of those parameter methods, instead of our workflow having a hard-coded path for our inputs, we can dynamically provide our input file names and clean things up in our workflow even further.

From:workflow { CUTADAPT(Channel.fromPath(~/sample1.fastq', checkIfExists: true))}

To:

workflow { CUTADAPT(Channel.fromPath(params.reads, checkIfExists: true))}



