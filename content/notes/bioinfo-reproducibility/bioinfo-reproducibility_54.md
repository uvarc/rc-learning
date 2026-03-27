---
title: Adding SLURM Options
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2750
menu: 
    bioinfo-reproducibility:
---

So, we need to let Nextflow know that we want to use SLURM to execute our processes and with do this by specifying SLURM as our executor. We can also use this to specify various other options – nextflow doesn't have explicit options for all possible slurm commands, so we can supplement with any additional options we need with 'clusterOptions.' Again, there's multiple ways to configure everything – you can also do global slurm options, but often different parts of the workflow are going to need different resources. And we could potentially specify these slurm options in the CUTADAPT process in our main.nf, but we're trying to keep that tidy and focused on the workflow logic.


Process {    withName: CUTADAPT {        beforeScript = '''        module purge        module load cutadapt

'''

executor = 'slurm'    queue = 'standard'    cpus = 2    mem = '16 GB'    time = '1h'    clusterOptions = '--account=hpc_build'

}



