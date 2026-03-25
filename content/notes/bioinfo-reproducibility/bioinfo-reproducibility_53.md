---
title: Loading Software – nextflow.config
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2700
menu: 
    bioinfo-reproducibility:
---


Again, we use a 'beforeScript' specific to the CUTADAPT process

Process {withName: CUTADAPT {    beforeScript = '''    module purge    module load cutadapt

'''

Now when we specifically run our CUTADAPT process, these commands will run before our script command and set up our process environment.  Ok, so now we have our software dialed in for cutadapt. But we need to think about where we are running these processes. By default, nextflow is running shell commands locally, so that means if we're just at the command line, we'd be running the processes on the login nodes, which is a no-no.

