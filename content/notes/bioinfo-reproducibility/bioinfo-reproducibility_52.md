---
title: Loading Software
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2650
menu: 
    bioinfo-reproducibility:
--- 

main.nf

Use a 'beforeScript' in the CUTADAPT process in main.nf

beforeScript runs specified shell command(s) before running the script command

Load the cutadapt module: beforeScript 'module load cutadapt'

Can also do other things like export variables or create directories

beforeScript """        module purge        module load cutadapt        mkdir results        export PATH="$PATH:/opt/tools"'    """


We can definitely load the software in our process, but we just cleaned that thing up, so let's put it somewhere better to keep our main.nf focused on workflow logic. To do this, let's go ahead and start to build a nextflow.config file.

