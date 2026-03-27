---
title: Workflow for CUTADAPT → BWA_ALIGN → FREEBAYES
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2900
menu: 
    bioinfo-reproducibility:
---

Here's how we could link the trim, align and variant calling together. So now we'll put it all together and run the entire workflow from end to end on the system.

workflow {

reads_ch = Channel.fromPath("${params.reads_dir}/*.fastq", checkIfExists:  true)

trimmed_ch = CUTADAPT(reads_ch)

aligned_ch = BWA_ALIGN(trimmed_ch)

FREEBAYES(aligned_ch)

}


