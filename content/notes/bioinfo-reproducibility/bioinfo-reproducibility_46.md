---
title: What to Update in Nextflow?
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2350
menu: 
    bioinfo-reproducibility:
---

So, looking at our HELLO process, what do we need to add? We already have a publishDir, an output, and script, so let's update those for cutadapt.

__process__  HELLO {    publishDir "results/" , mode: "copy"

__    __ output:    path 'hello.txt'    script:    """    echo 'Hello world!' > hello.txt    """}


