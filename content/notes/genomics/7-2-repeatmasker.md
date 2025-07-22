---
title: RepeatMasker
date: 2025-7-10T00:00:00
type: docs 
weight: 450
menu: 
    genomics:
        parent: Software Demos
---

Resolving repeats



* Repeats longer than reads
* RepeatMasker - http://repeatmasker.org
  * -screens DNA sequences for interspersed repeats
  * Soft & Hard maskingSoft masking indicates masked regions by using lower-case letters. Hard masking (-hardmask option) overwrites masked regions with a wildcard letter, N for nucleotides or X for proteins
  * AGAGGCGGGGCGGGAGTCCCCGTCCTGCCGCCGCCCccgagcccagccgcccgccatgtcccgccgccggggtcCACGTGCATGCGTGCCGAGTGAACTCTCCCGCCCCGACGCGCtccggctccgggc
  * Why mask a genome?
  * Prevent ambiguous alignments to regions of high similarity
---

The low complexity may be preconditioned by strong inequality in nucleotide content (biased composition), by tandem or dispersed repeats or by palindrome-hairpin structures, as well as by a combination of all these factors.
detailed annotation of the repeats that are present in the query sequence as well as a modified version of the query sequence in which all the annotated repeats have been masked (default: replaced by Ns). Currently over 56% of human genomic sequence is identified and masked by the program



Running RepeatMasker - slurm script

__RepeatMasker_slurm_submit.sh__  __ __

#!/bin/bash

#SBATCH -A hpc_training                     # account name (--account)

#SBATCH -p standard                         # partition/queue (--partition)

#SBATCH --nodes=1                           # number of nodes

#SBATCH --ntasks=1                          # 1 task – how many copies of code to run

#SBATCH --cpus-per-task=4                   # total cores per task – for multithreaded code

##SBATCH --mem=3200                         # total memory (Mb) *Note ##comment

#SBATCH -t 01:00:00                         # time limit: 1-hour

#SBATCH -J RepeatMasker-test                # job name

#SBATCH -o RepeatMasker-test-%A.out         # output file

#SBATCH -e RepeatMasker-test-%A.err         # error file

#SBATCH --mail-user=dtriant@virginia.edu    # where to send email alerts

#SBATCH --mail-type=ALL                     # receive email when starts/stops/fails

__module purge__   # good practice to purge all modules

__module load__  gcc/11.4.0

__module load__  openmpi/4.1.4

__module load__  repeatmasker/4.1.9

cd /project/rivanna-training/genomics-hpc/RepeatMasker

__RepeatMasker__  genome_raw.fasta -lib Muco_library_EDTA.fasta -gff

---

Slurm – resource manager, how you can run your codes

![](img/genomics_65.png)

Masked sequence files

Repeat statistics table

.gff file

https://www.repeatmasker.org/

Interactive searching among commonly available genomes:

https://www.repeatmasker.org/cgi-bin/AnnotationRequest

Downloading raw annotation:

https://www.repeatmasker.org/genomicDatasets/RMGenomicDatasets.html



