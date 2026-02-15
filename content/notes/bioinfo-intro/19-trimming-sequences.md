---
title: Trimming Sequences
date: 2025-08-23T03:19:53Z
date: 2025-08-23-03:19:53Z
type: docs 
weight: 1000
menu: 
    bioinfo-intro:
        parent: Sequence Processing & Alignment

---

Before aligning sequencing reads, it’s important to remove adapter sequences and low-quality bases.

{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_44.png caption="Illumina documentation listing adapter and primer sequences commonly trimmed from raw reads." width=80% height=80% >}}

[Read more about Illumina adapter sequences](https://support-docs.illumina.com/SHARE/AdapterSequences/Content/SHARE/AdapterSeq/Nextera/SequencesNextera_Illumina.htm)


### Cutadapt 

[Cutadapt](https://cutadapt.readthedocs.io/en/stable/) is a program that finds and removes adapter sequences, primers, poly-A tails and other types of unwanted sequence from your high-throughput sequencing reads.

Essentially, Cutadapt trims adapters from short read Illumina data. 

[Documentation](https://cutadapt.readthedocs.io/en/stable/)

{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_42.png width=85% height=85% >}}

The above image shows a FASTQC `.html` report indicating that adapter sequences are still present in the reads. In this case, you would use **Cutadapt** to trim the adapters. 

### Using Cutadapt

Check available versions on the cluster:

```bash
$ module spider cutadapt
```

Output:

`cutadapt: cutadapt/4.9`


This module can be loaded directly: 

```bash
module load cutadapt/4.9
```


Sample run command:

```bash
$ cutadapt -a CTGTCTCTTATACACATCT -o SRR2584866-trimmed_1.fastq  SRR2584866_1.fastq
```

Output Summary (Cutadapt 4.9, Python 3.12.9):

```text
Processing single-end reads on 1 core ...

Done           00:00:07     2,768,398 reads @   2.8 µs/read;  21.79 M reads/minute

Finished in 7.626 s (2.755 µs/read; 21.78 M reads/minute).

=== Summary ===

Total reads processed:               2,768,398

Reads with adapters:                   683,198 (24.7%)

Reads written (passing filters):     2,768,398 (100.0%)

Total basepairs processed:   415,259,700 bp

Total written (filtered):    377,219,215 bp (90.8%)
```


### Impact 

After adapter trimming, run FASTQC again to check the quality of the results.

```bash
$ module spider FASTQC 

$ module load fastqc/0.12.1

$ module list # check what’s loaded (did you remember to `module purge` first?)

$ mkdir fastqc-out-trimmed

$ fastqc -t 4 -o fastqc-out-trimmed  ecoli-fastq/SRR2584866-trimmed_1.fastq
```

Before trimming: 

{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_49.png width=70% height=70% >}}

After trimming:

{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_51.png width=70% height=70% >}}

As you can see, adapter sequences have been successfully removed.
