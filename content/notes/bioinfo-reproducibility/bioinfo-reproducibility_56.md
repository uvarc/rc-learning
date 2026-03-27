---
title: Extend to CUTADAPT → BWA_ALIGN → FREEBAYES
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2850
menu: 
    bioinfo-reproducibility:
---


* Same rules apply – largely rinse and repeat for additional processes
* Create processes for each step: inputs/outputs, commands, etc.
* Software and slurm options in nextflow.config
* Main difference is our workflow - more processes and channels
  * Send channel into process
  * Process produces output
  * Output becomes new channel for next process.


With Nextflow, channels carry data and processes do work on that data. You link them together by sending a channel into a process, and if that process produces output, its output can become a new channel for the next process.

