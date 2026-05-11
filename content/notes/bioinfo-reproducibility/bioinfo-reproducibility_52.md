---
title: Loading Software
date: 2026-03-25T19:08:46Z
type: docs 
weight: 2650
menu: 
    bioinfo-reproducibility:
--- 

main.nf

Use a `beforeScript` in the `CUTADAPT` process in `main.nf`
- `beforeScript` runs specified shell command(s) before running the script command
- Load the `cutadapt` module: `beforeScript 'module load cutadapt'`
- Can also do other things like export variables or create directories

```groovy
beforeScript """
module purge
module load cutadapt
mkdir results
export PATH="$PATH:/opt/tools"
"""
```

We can definitely load the software in our process, but we just cleaned that thing up, so let's put it somewhere better to keep our main.nf focused on workflow logic. To do this, let's go ahead and start to build a nextflow.config file.


## Loading Software - nextflow.config
Again, we use a `beforeScript` specific to the `CUTADAPT` process

```groovy
process {
  withName: CUTADAPT {
    beforeScript = '''
    module purge
    module load cutadapt
    '''
  }
}
```

Now when we specifically run our CUTADAPT process, these commands will run before our script command and set up our process environment. Now we haveour software dialed in for cutadapt. But we need to think about where we are running these processes. By default, nextflow is running shell commands locally, so that means if we're just at the command line, we'd be running the processes on the login nodes, which is discouraged


## Adding SLURM Options
```groovy
process {
  withName: CUTADAPT {
    beforeScript = '''
    module purge
    module load cutadapt
    '''
    executor = 'slurm'
    queue = 'standard'
    cpus = 2
    mem = '16 GB'
    time = '1h'
    clusterOptions = '--account=hpc_build'
  }
}
```

We need to let Nextflow know that we want to use SLURM to execute our processes and with do this by specifying SLURM as our executor. We can also use this to specify various other options - nextflow doesn't have explicit options for all possible slurm commands, so we can supplement with any additional options we need with 'clusterOptions.'


## Now we have
- Workflow logic in main.nf
- Software and slurm options in nextflow.config


## Extend to CUTADAPT -> BWA_ALIGN -> FREEBAYES
- Same rules apply - largely rinse and repeat for additional processes
- Create processes for each step: inputs/outputs, commands, etc.
- Software and slurm options in nextflow.config
- Main difference is our workflow - more processes and channels
  * Send channel into process
  * Process produces output
  * Output becomes new channel for next process.


With Nextflow, channels carry data and processes do work on that data. You link them together by sending a channel into a process, and if that process produces output, its output can become a new channel for the next process.


## Workflow for CUTADAPT -> BWA_ALIGN -> FREEBAYES
Here's how we could link the trim, align and variant calling together. So now we'll put it all together and run the entire workflow from end to end on the system.

```groovy
workflow {
 reads_ch = Channel.fromPath("${params.reads_dir}/*.fastq", checkIfExists: true)
 trimmed_ch = CUTADAPT(reads_ch)
 aligned_ch = BWA_ALIGN(trimmed_ch)
 FREEBAYES(aligned_ch)
}
```