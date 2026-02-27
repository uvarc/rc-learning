---
title: Bowtie2 - Sequence Alignment
date: 2025-08-23T03:19:53Z
type: docs 
weight: 1150
menu: 
    bioinfo-intro:
        parent: Sequence Processing & Alignment
---

After trimming adapters and low-quality sequences with Cutadapt, the next step is to align the cleaned reads to a reference genome using Bowtie2.

{{< figure src=/notes/bioinfo-intro/img/bowtie.png width=75% height=75% caption="After alignment with tools such as Bowtie2, the mapped reads can be visualized to assess coverage, variants, and alignment quality across sequencing platforms." >}}

Alignment is often the time-limiting step. 

**Bowtie** can be used to align short reads. 

__End-to-end alignment example__

The following is an "end-to-end" alignment because it involves all the characters in the read (default).

{{< figure src=/notes/bioinfo-intro/img/bowtieexample.png width=60% height=60% >}}

__Local alignment example__

The following is a "local" alignment because some of the characters at the ends of the read do not participate. In this case, 4 characters are omitted (or "soft trimmed" or "soft clipped") from the beginning and 3 characters are omitted from the end.

{{< figure src=/notes/bioinfo-intro/img/read2bowtie.png width=60% height=60% >}}

[Bowtie2 Documentation](https://bowtie-bio.sourceforge.net/bowtie2/index.shtml)

### Loading and Running the Module


```bash
$ module spider bowtie2
$ module load bowtie2   # which version loaded?
$ module list # did you purge?
$ module spider bowtie2/2.5.4 # sometimes more detail about specific version
$ bowtie2 # to run
```

`$ bowtie2 -help` outputs a summary of command-line options.

**Command Structure:**

bowtie2 [options]* -x <bt2-idx> {-1 <m1> -2 <m2> | -U <r>} [-S <  sam  >]

`-x <bt2-idx>`  Index filename prefix (minus trailing `.X.bt2`).

- NOTE: Bowtie 1 and Bowtie 2 indexes are not compatible.

`-1 <m1>`       Files with #1 mates, paired with files in `<m2>`.

- Could be `gzip`'ed (extension: `.gz`) or `bzip2`'ed (extension: `.bz2`).

`-2 <m2>`       Files with #2 mates, paired with files in `<m1>`.

- Could be `gzip`'ed (extension: `.gz`) or `bzip2`'ed (extension: `.bz2`).

`-U <r>`        Files with unpaired reads.

- Could be `gzip`'ed (extension: `.gz`) or `bzip2`'ed (extension: `.bz2`).

`-S <sam>`      File for SAM output (default: stdout)

`<m1>`, `<m2>`, `<r>` can be comma-separated lists (no whitespace) and can be specified many times.  

E.g. `-U file1.fq,file2.fq -U file3.fq`.

### Using Bowtie2

First, build an index for your genome using sample data provided with the bowtie2 download. 

`$ ls example`

Output:

index/  
reads/  
example/  

This command creates the index:

`$ __ bowtie2-build __ lambda_virus.fa lambda_virus	*.bt2 index files`

These index files will now be available in your directory:

`lambda_virus.4.bt2`  
`lambda_virus.3.bt2`  
`lambda_virus.1.bt2`  
`lambda_virus.2.bt2`  
`lambda_virus.rev.1.bt2`  
`lambda_virus.rev.2.bt2`  

Next, align the reads using this command:

`$  __bowtie2__  -x lamda_virus -1 lamda-reads_1.fastq -2 lamda-reads_2.fastq -S lamda.sam`


**Bowtie2 Output**

```
Building a SMALL index

10000 reads; of these:

<span style="color:#ff0000"> __Concordant alignment__ </span>

10000 (100.00%) were paired; of these:

834 (8.34%) aligned concordantly 0 times

__9166 (91.66%) aligned concordantly exactly 1 time__

0 (0.00%) aligned concordantly >1 times

----

<span style="color:#ff0000"> __Discordant alignment__ </span>

__834 pairs aligned concordantly 0 times; of these:__

42 (5.04%) aligned discordantly 1 time

----

<span style="color:#ff0000"> __The rest of the reads either align as singles __ </span>

__792 pairs aligned 0 times concordantly or discordantly__ ; of these:

1584 mates make up the pairs; of these:

1005 (63.45%) aligned 0 times

579 (36.55%) aligned exactly 1 time

0 (0.00%) aligned >1 times

__94.97% overall alignment rate__
```

The result summary is divided into 3 sections: 

1. Concordant alignment 

This shows the reads that were aligned properly as pairs. In this example, 91.66% of reads were aligned concordantly. 


2. Discordant alignment 

This shows the reads that did not align in the expected configuration. In this example, of the remaining 8.34% of reads, 792 reads align discordantly. That is to say, of the non-concordant fraction, 5.04% of reads (42 reads) align discordantly. 

3. Single or unpaired alignments 

Alignments, whether concordant or discordant, are aligned in paired-end mode. The rest of the reads either align as singles (i.e. `Read1` in one locus and `Read2` in a different locus, or one mate aligned and the other unaligned) or may not align at all. The reads in this section are 

$$Total - (Concordant + Discordant)$$ 

In this example, that is 

$$1000 - (9166 + 42) = 792$$ 

To reach the overall alignment, count the mates in total (i.e. mates aligned in paired and mates aligned in single fashion). In this example, that would be:

$$(9166 * 2) + (42 * 2) + 579 = 18995$$ 

That is 18885 mates aligned of total (10000 x2) mates, which is 94.97%.


