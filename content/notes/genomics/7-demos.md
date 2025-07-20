---
title: Software Demos
date: 2025-7-10T00:00:00
type: docs 
weight: 350
menu: 
    genomics:
---





__BUSCO: assessing genome assembly and annotation completeness with single-copy __  __orthologs__

Bioinformatics. 2015;31(19):3210-3212. doi:10.1093/bioinformatics/btv351

![](img/genomics_58.png)

BUSCO assessment workflow and relative run-times

Quality of genome vs. completeness



![](img/genomics_60.png)

https://busco.ezlab.org

Running Busco - interactive

Run interactively - good for testing

$ ijob -c 1 -A hpc_training -p standard -v

Copy files:

$ pwd

$ cp /project/rivanna-training/genomics-hpc .

. -> copy it here

$ ls -lh  check to make sure all files transferred



$ module spider busco

$ module load busco/5.8.2

$ module list

$  __busco__  -i Cyglarus-subset.fasta -o Lep-Blue_out -m genome -l lepidoptera_odb10

- l lineage dataset - downloads first time used

- o make new output directory or will throw an error if exists

What kind of files am I working with? Let's look them up!

How many sequences in the file?

Running Busco – slurm script

__busco_slurm_submit.sh__  __ __

#!/bin/bash

#SBATCH -A hpc_training                    # account name (--account)

#SBATCH -p standard                        # partition/queue (--partition)

#SBATCH --nodes=1                          # number of nodes

#SBATCH --ntasks=1                         # 1 task – how many copies of code to run

#SBATCH --cpus-per-task=4                  # total cores per task – for multithreaded code

##SBATCH --mem=3200                        # total memory (Mb) *Note ##comment

#SBATCH -t 01:00:00                        # time limit: 1-hour

#SBATCH -J busco-test                      # job name

#SBATCH -o busco-test-%A.out               # output file

#SBATCH -e busco-test-%A.err               # error file

#SBATCH --mail-user=dtriant@virginia.edu   # where to send email alerts

#SBATCH --mail-type=ALL                    # receive email when starts/stops/fails

__module purge__   # good practice to purge all modules

__module load__  busco/5.8.2

cd /project/rivanna-training/genomics-hpc/busco

#make new output directory or rename each time or throws an error if already exists!

__busco__  -i Cyglarus-subset.fasta -o Lep-Blue_out -m genome -l lepidoptera_odb10

---

Slurm – resource manager, how you can run your codes

Running Busco – slurm script

mail option output:

Job started: Slurm Job_id=4972539 Name=busco-test Began, Queued time 00:00:46

![](img/genomics_62.png)



Running Busco – slurm script

$ sbatch busco_slurm_submit.sh

Submitted batch job 5788712

Job monitoring:

$ squeue -u user_id  # checks job status

PD - pending

R - running

CG - exiting

Should have same output files as when run interactively



If we place our contigs from largest to smallest on the genome,

50% of the genome in contigs as long as or larger than N50 value

Example:	1 Mbp genome

N50 size = 30 kbp

(300k+100k+45k+45k+30k = 520k >= 500kbp)

__A greater N50 is usually a sign of assembly improvement__

Comparable with genomes of similar size

Genome composition can bias comparisons

Low L50 vs High N50

---

L50 length – high L50 vs low N50 scaffolds Fewer scaffolds because of stringency or length of fragments

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



Running StringTie

StringTie aligns bulk RNA-Seq reads to a reference genome and estimates abundance

Based on FPKM values (Fragments Per Kilobase of transcript per Million mapped reads)

Can use outputs to estimate expression values

https://ccb.jhu.edu/software/stringtie/


Running StringTie – slurm script

__stringtie__  ___slurm_submit.sh__  __ __

#!/bin/bash

#SBATCH -A hpc_training                    # account name (--account)

#SBATCH -p standard                        # partition/queue (--partition)

#SBATCH --nodes=1                          # number of nodes

#SBATCH --ntasks=1                         # 1 task – how many copies of code to run

#SBATCH --cpus-per-task=1                  # total cores per task – for multithreaded code

##SBATCH --mem=3200                        # total memory (Mb) *Note ##comment

#SBATCH -t 00:20:00                        # time limit: 20 min

#SBATCH -J stringtie-test                  # job name

#SBATCH -o stringtie-test-%A.out           # output file

#SBATCH -e stringtie-test-%A.err           # error file

#SBATCH --mail-user=dtriant@virginia.edu   # where to send email alerts

#SBATCH --mail-type=ALL                    # receive email when starts/stops/fails

__module purge__   # good practice to purge all modules

__module load __ stringtie/2.2.1

cd /project/rivanna-training/genomics-hpc/stringtie/tests_3 # working directory

__stringtie__  --mix -G mix_guides.gff -o mix_reads_guided.out.gtf mix_short.bam mix_long.bam

# -- mix both short and long reads aligning

# -G reference annotation for guided alignment with short (1st) & long (2nd) read bam alignment files

# generates .gtf file with assembled transcripts and expression levels

---

Slurm – resource manager, how you can run your codes

Interactively

Loading modules

Downloading software locally



Running StringTie

What if the module is not installed?

- Submit a request via ticket system - waiting time, popular request

- Install it yourself and run locally

- Run from your directory but submitted to computing nodes via

slurm script

Installing software “locally”

https://github.com/gpertea/stringtie

$ git clone https://github.com/gpertea/stringtie

$ cd stringtie

$ make -j4 release



Running StringTie

Installing software “locally”

https://github.com/gpertea/stringtie

$ git clone https://github.com/gpertea/stringtie

$ cd stringtie

$ make -j4 release

Look for a README file - often has download/installation instructions

$ ls -lh stringtie

-rwxrwx---+ 1 dat2g rc-staff 14M Jun  3 22:37 stringtie

- file permissions


Running StringTie

File permissions

$ ls -lh stringtie -rwxrwx---+ 1 dat2g rc-staff 14M Jun  3 22:37 stringtie

- file permissions

-rw-r--r-- 12 linuxize users 12.0K Apr  8 20:51 filename.txt

|[-][-][-]-   [------] [---]

| |  |  | |      |       |

| |  |  | |      |       +-----------> 7. Group

| |  |  | |      +-------------------> 6. Owner

| |  |  | +--------------------------> 5. Alternate Access Method

| |  |  +----------------------------> 4. Others Permissions

| |  +-------------------------------> 3. Group Permissions

| +----------------------------------> 2. Owner Permissions

+------------------------------------> 1. File Type. - regular file, d directory

File not readable

r    File is readable

w   File is  writeable

x   File is  executable

![](img/genomics_71.png)

Running StringTie

Installing software “locally”

https://github.com/gpertea/stringtie

$ git clone https://github.com/gpertea/stringtie

$ cd stringtie

$ make -j4 release

To download and test sample data:

$ ./run_test.sh          Is there a README there too?

We are going to run Test 7 - Mixed reads with annotation guides:

$  __stringtie__  --mix -G mix_guides.gff  -o mix_reads_guided.out.gtf mix_short.bam mix_long.bam




Running StringTie

./run_test.sh - to download and test sample data

__Test 1__ : Short reads

Running: ../stringtie  -o short_reads.out.gtf short_reads.bam

__Test 2__ : Short reads and super-reads

Running: ../stringtie  -o short_reads_and_superreads.out.gtf short_reads_and_superreads.bam

__Test 3__ : Short reads with annotation guides

Running: ../stringtie -G mix_guides.gff -o short_guided.out.gtf mix_short.bam

__Test 4__ : Long reads

Running: ../stringtie -L -o long_reads.out.gtf long_reads.bam

__Test 5__ : Long reads with annotation guides

Running: ../stringtie -L -G human-chr19_P.gff -o long_reads_guided.out.gtf long_reads.bam

__Test 6__ : Mixed reads

Running: ../stringtie --mix -o mix_reads.out.gtf mix_short.bam mix_long.bam

__Test 7: Mixed reads with annotation guides__

__Running: ../__  __stringtie__  __ --mix -G __  __mix_guides.gff__  __ -o __  __mix_reads_guided.out.gtf__  __ __  __mix_short.bam__  __ __  __mix_long.bam__

__Test 8__ : Short reads with -N

Running: ../stringtie -N -G mix_guides.gff -o mix_short_N_guided.out.gtf mix_short.bam

__Test 9__ : Short reads with --nasc

Running: ../stringtie --nasc -G mix_guides.gff -o mix_short_nasc_guided.out.gtf mix_short.bam

Running StringTie

Note your pathway

/project/rivanna-training/genomics-hpc/stringtie/stringtie

$ cd tests_3

$  __stringtie__  --mix -G mix_guides.gff  -o mix_reads_guided.out.gtf mix_short.bam mix_long.bam

- m - mixed reads short/long

- G - .gff file - annotation guide

- order of bams important: short first, long second

- .gtf output file


PacBio - SMRTLink

SMRTLink - PacBio analysis software

New Revio sequencing machine in SOM genomics core

$ module spider SMRT

$ module load smrtlink/25.2.0

-LOTS of tools available for sequence

analysis and manipulation

-  _bam2fasta, bam2fastq, _  _blasr_  _, _  _pbmm_  _, _  _pbalign_  _, isoseq3, _

![](img/genomics_74.png)

https://docslib.org/doc/175362/smrt%C2%AE-tools-reference-guide-v8-0

Research Computing resources

UVA Research Computing Learning Portal:

https://learning.rc.virginia.edu

Using UVA’s HPC system from the terminal:

https://learning.rc.virginia.edu/notes/hpc-from-terminal/

HPC orientation session and office hours:

https://www.rc.virginia.edu/support/#office-hours




Additional bioinformatics software installed

Trimmomatic: http://www.usadellab.org/cms/?page=trimmomatic 
Qualimap: http://qualimap.conesalab.org 
HISAT2: https://daehwankimlab.github.io/hisat2
StringTie: https://ccb.jhu.edu/software/stringtie
STAR: https://github.com/alexdobin/STAR
Trinity: https://github.com/trinityrnaseq/trinityrnaseq/wiki
RSEM: https://github.com/deweylab/RSEM
DESeq2: https://bioconductor.org/packages/release/bioc/html/DESeq2.html
Salmon: https://salmon.readthedocs.io/en/latest/salmon.html
edgeR: https://bioconductor.org/packages/release/bioc/html/edgeR.html
bedtools: https://bedtools.readthedocs.io/en/latest/index.html
vcftools: https://vcftools.github.io
picard: https://broadinstitute.github.io/picard/
canu: https://canu.readthedocs.io/en/latest
bcftools: https://samtools.github.io/bcftools/bcftools


Genomics Software

Verkko Telomere-to-telomere assemblies

https://github.com/marbl/verkko

FALCON & FALCON-unzip - PacBio

https://pb-falcon.readthedocs.io/en/latest/about.html

MaSuRCA - Illumina, PacBio, Nanopore

https://github.com/alekseyzimin/masurca

SPAdes - Illumina, PacBio, Nanopore

https://github.com/ablab/spades

Hinge - PacBio, Nanopore

https://github.com/HingeAssembler/HINGE

Abyss - Illumina

https://github.com/bcgsc/abyss

Shasta - Nanopore

https://github.com/paoloshasta/shasta

