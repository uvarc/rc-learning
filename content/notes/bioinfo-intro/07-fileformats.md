---
title: File Formats
date: 2025-08-23-03:19:53Z
type: docs 
weight: 400
menu: 
    bioinfo-intro:
        parent: Bioinformatics
---

{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_16.png caption="Source: https://xkcd.com/927/" width=80% height=80% >}}

The format name usually denotes the file suffix. 

**FASTA** files (suffix: `.fasta`, `.fna`, `.fa`) store sequencing data. 

**FASTQ** files (suffix: `.fastq`) include sequencing data and quality scores. 

**SAM/BAM** files (suffix: `.sam`/`.bam`) were developed for next-generation sequencing (NGS) data. SAM stands for Sequence Alignment Map. These files are used to store alignment information. 

**VCF** (suffix: `.vcf`) stands for Variant Call Format. These files are used to store information about genetic variants. [Read More](https://samtools.github.io/hts-specs/VCFv4.2.pdf)

**GFF3** (suffix: `.gff3`) stands for Generic Feature Format (version 3). These files are used to store information about genomic features. [Read More](https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md) 

**BED** (suffix: `.bed`) stands for Browser Extensible Data format. These files are used to store genomic regions. [Read More](https://github.com/arq5x/bedtools2)
  
### FASTA Format 

A FASTA file begins with a header line, indicated by the `>` symbol, that contains an identifier and optional description The following lines contain the biological sequence itself.


<span style="color:#ff0000"> __>__ </span> NP_000552.2 Human glutathione transferase M1 (GSTM1) ```
MPMILGYWDIRGLAHAIRLLLEYTDSSYEEKKYTMGDAPDYDRSQWLNEKFKLGLDFPNLPYLIDGAHKITQSNAILCYIARKHNLCGETEEEKIRVDILENQTMDNHMQLGMICYNPEFEKLKPKYLEELPEKLKLYSEFLGKRPWFAGNKITFVDFLVYDVLDLHRIFEPKCLDAFPNLKDFISRFEGLEKISAYMKSSRFLPRPVFSKMAVWGNK```


### FASTQ Format

FASTQ files are helpful for base calling, quality control, and trimming. 

Most sequencing tools return data in FASTQ format with quality scores included (ASCII code). 

FASTQ files contain four lines: 
1. ID, beginning with `@`
2. Sequence
3. Description line (typically a `+`)
4. Base qualities in ASCII format

```plaintext
@SEQ_ID
GATTTGGGGTTCAAAGCAGTATCGATCAAATA
+
!''*((((***+))%%%++)(%%%%).1***-
```

**FASTQ File Example: Multiple Reads**

```plaintext
@M00747:32:000000000-A16RG:1:1112:15153:29246 1:N:0:1
TCGATCGAGTAACTCGCTGCTGTCAGACTGGTTTTTGGTCGATCGACTATTGTTTCAGTCGCAAGAATATTGTGTCCAGTCGATCGACTGAATTCTGCTGTACGGCCACGGCGGATGCACGGTACAGCAGGCTCAGACGGATTAAACTGTT
+ 
5=9=9<=9,-5@<<55>,6+8AC>EE.88AE9CDD7>+7.CC9CD+++5@=-FCCA@EF@+**+*--55--AA---AA-5A<9C+3+<9)4++=E=+===<D94)00=9)))2@624(/(/2/-(.(6;9(((((.(.'((6-66<6(///
@M00747:32:000000000-A16RG:1:1112:15536:29246 1:N:0:1
GTAAAATTGAGGTAAATTGTGCGGAATTTAGCAATACCGTTTTTTTTATTATCACCGGATATCTATTCTGCTGTACGGCCAAGGAGGATGTACGGTACAGCAGGTGCGAACTCACTCCGACGCTCAAGTCAGTGACTTAATGATAAGCGTG
+
?????<BBBBBB5<?BFFFFFFECHEFFECCFF?9AAC>7@FHHHHHHFG?EAFGF@EEDEHHDGHHCBDFFGDFHF)<CCD@F,+3=CFBDFHBD++??DBDEEEDE:):CBEEEBCE68>?))5?**0?:AE*A*0//:/*:*:**.0)
@M00747:32:000000000-A16RG:1:1112:15513:29246 1:N:0:1
GCTAGTCTTGTGTTTAGTTTTATGTTTTGCATGTTGTAACGGATTCATAAACATAGGTGTTTGTTTCTTTTTATGGTTGTACAATTTGGCCCTAAGGCCCTACACTTACTTGTTTGTTTCTTTTATGGTACGACATTTGAGTGGTGGTTGA
+
```

