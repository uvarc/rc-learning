---
title: Running FASTQC Interactively
date: 2025-08-23T03:19:53Z
date: 2025-08-23-03:19:53Z
type: docs 
weight: 850
menu: 
    bioinfo-intro:
        parent: Running Jobs
---

Running FASTQC interactively is good for testing. 

Sample terminal command to run interactive jobs:

```bash
$ ijob -c 1 -A hpc_training -p standard -v
```

### Copying workshop files 

```bash
$ pwd

$ cp /project/rivanna-training/bioinformatics-hpc .
# . means copy it in your current working directory 

# check to make sure all files transferred:
$ ls -lh  
```

### Running FASTQC 

```bash
$ module spider fastqc

$ module load fastqc/0.12.1

$ module list

$ fastqc -t 4 -o fastqc-out ./fastqc/ecoli-fastq/SRR258*fastq.*
```

Notes: 
* You need to know the path to your FASTQ files.
* The `*` wildcard (`*fastq`) matches all FASTQ files in the folder. 
* You need to specify an output directory (`fastqc-out`) where the results will be saved.  
* `-t 4` sets the number of threads (4).

### FASTQC Outputs

The above command produces `.html` reports. 

{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_34.png caption="Job output - Quality Scores" width=70% height=70% >}}


{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_36.png caption="Job output - Adapter Content. The red X indicates that adapter contamination is detected." width=80% height=80% >}}

