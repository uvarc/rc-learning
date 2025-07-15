---
title: Sequence File Formats
date: 2025-7-10T00:00:00
type: docs 
weight: 200
menu: 
    genomics:
---


![](img/genomics_25.png)

http://xkcd.com/927/

![](img/genomics_26.png)

File formats – format name usually denotes suffix



* FASTA (file suffix: .fasta, .fna, .fa)
* FASTQ (file suffix: .fastq)
  * - Quality scores
* SAM/BAM (file suffix: .sam/.bam)
  * - Developed for NGS data
  * - Sequence Alignment Map
  * - Stores alignment information


![](img/genomics_27.png)


* VCF (file suffix: .vcf)
* - Variant Call Format
* https://samtools.github.io/hts-specs/VCFv4.2.pdf
* GFF3 (file suffix: .gff3)
  * - Generic Feature Format, version 3
* GTF (file suffix: .gtf)
  * General Feature Format
  * similar to .gff3 but contains additional gene annotation information
  * http://mblab.wustl.edu/GTF22.html


https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md

![](img/genomics_28.png)


__>__ NP_000552.2 Human glutathione transferase M1 (GSTM1) MPMILGYWDIRGLAHAIRLLLEYTDSSYEEKKYTMGDAPDYDRSQWLNEKFKLGLDFPNLPYLIDGAHKITQSNAILCYIARKHNLCGETEEEKIRVDILENQTMDNHMQLGMICYNPEFEKLKPKYLEELPEKLKLYSEFLGKRPWFAGNKITFVDFLVYDVLDLHRIFEPKCLDAFPNLKDFISRFEGLEKISAYMKSSRFLPRPVFSKMAVWGNK

![](img/genomics_29.png)

---

First time opened assembly

File formats - FASTQ



* Base calling, quality control, trimming
* Most data returned in FASTQ format with quality scores included (ASCII code)


__@SEQ_ID GATTTG__  __GG__  __GTTCAA__  __AG__  __CAGTAT__  __CG__  __ATCAAA__  __T__  __A__

__+__

__!''*((__  __((__  __***+))__  __%%__  __%++)(%__  __%%__  __%).1***__  __-__

id

sequence

description line base qualities

[https://help.basespace.illumina.com/files-used-by-basespace/quality-scores](https://help.basespace.illumina.com/files-used-by-basespace/quality-scores)

File formats - FASTQ

__@M00747__  __:32:000000000-A16RG:1:1112:15153:29246 1:N:0:1__

__TCGATCGAGTAACTCGCTGCTGTCAGACTGGTTTTTGGTCGATCGACTATTGTTTCAGTCGCAAGAATATTGTGTCCAGTCGATCGACTGAATTCTGCTGTACGGCCACGGCGGATGCACGGTACAGCAGGCTCAGACGGATTAAACTGTT__

__+__

__5=9=9<=9,-5@<<55>,6+8AC>EE.88AE9CDD7>+7.CC9CD+++5@=-FCCA@EF@+**+*--55--AA---AA-5A<9C+3+<9)4++=E=+===<D94)00=9)))2@624(/(/2/-(.(6;9(((((.(.'((6-66<6(///__

@M00747:32:000000000-A16RG:1:1112:15536:29246 1:N:0:1

GTAAAATTGAGGTAAATTGTGCGGAATTTAGCAATACCGTTTTTTTTATTATCACCGGATATCTATTCTGCTGTACGGCCAAGGAGGATGTACGGTACAGCAGGTGCGAACTCACTCCGACGCTCAAGTCAGTGACTTAATGATAAGCGTG

+

?????<BBBBBB5<?BFFFFFFECHEFFECCFF?9AAC>7@FHHHHHHFG?EAFGF@EEDEHHDGHHCBDFFGDFHF)<CCD@F,+3=CFBDFHBD++??DBDEEEDE:):CBEEEBCE68>?))5?**0?:AE*A*0//:/*:*:**.0)

@M00747:32:000000000-A16RG:1:1112:15513:29246 1:N:0:1

GCTAGTCTTGTGTTTAGTTTTATGTTTTGCATGTTGTAACGGATTCATAAACATAGGTGTTTGTTTCTTTTTATGGTTGTACAATTTGGCCCTAAGGCCCTACACTTACTTGTTTGTTTCTTTTATGGTACGACATTTGAGTGGTGGTTGA

+

SAM/BAM Sequence Alignment



* Alignment file - provides context for raw data
  * - Eleven columns, tab delimited
  * - One alignment record per line
* SAM is plain-text (human readable)
* BAM is a binary format
* SAMTools - suite of utilities for SAM/BAM files
* Picard - tools for sequencing data
* samtools: http://samtools.sourceforge.net
* Picard: https://broadinstitute.github.io/picard/


GTF - Gene Transfer Format

# stringtie --mix -G mix_guides.gff -o mix_reads_guided.out.gtf mix_short.bam mix_long.bam

# StringTie version 2.2.1

chr21   StringTie       transcript      46635595        46661278        1000    +       .       gene_id "STRG.1"; transcript_id "STRG.1.1"; reference_id "rna81955"; ref_gene_id "gene38337"; ref_gene_name "PRMT2"; cov "130.532318"; FPKM "106399.437500"\

; TPM "231113.671875";

chr21   StringTie       exon    46635595        46635763        1000    +       .       gene_id "STRG.1"; transcript_id "STRG.1.1"; exon_number "1"; reference_id "rna81955"; ref_gene_id "gene38337"; ref_gene_name "PRMT2"; cov "49.223850";

chr21   StringTie       exon    46636896        46636990        1000    +       .       gene_id "STRG.1"; transcript_id "STRG.1.1"; exon_number "2"; reference_id "rna81955"; ref_gene_id "gene38337"; ref_gene_name "PRMT2"; cov "106.469635";

chr21   StringTie       exon    46643535        46643639        1000    +       .       gene_id "STRG.1"; transcript_id "STRG.1.1"; exon_number "3"; reference_id "rna81955"; ref_gene_id "gene38337"; ref_gene_name "PRMT2"; cov "88.297432";

chr21   StringTie       exon    46644306        46644488        1000    +       .       gene_id "STRG.1"; transcript_id "STRG.1.1"; exon_number "4"; reference_id "rna81955"; ref_gene_id "gene38337"; ref_gene_name "PRMT2"; cov "100.436783";

chr21   StringTie       exon    46648458        46648619        1000    +       .       gene_id "STRG.1"; transcript_id "STRG.1.1"; exon_number "5"; reference_id "rna81955"; ref_gene_id "gene38337"; ref_gene_name "PRMT2"; cov "112.520607";

chr21   StringTie       exon    46649575        46649739        1000    +       .       gene_id "STRG.1"; transcript_id "STRG.1.1"; exon_number "6"; reference_id "rna81955"; ref_gene_id "gene38337"; ref_gene_name "PRMT2"; cov "119.953224";

chr21   StringTie       exon    46658745        46661278        1000    +       .       gene_id "STRG.1"; transcript_id "STRG.1.1"; exon_number "7"; reference_id "rna81955"; ref_gene_id "gene38337"; ref_gene_name "PRMT2"; cov "148.682266";

![](img/genomics_30.png)

Reference Genome

Reference:

Multiple sequencing technologies

Complete representation

Minimal sequencing gaps

Higher cost

Conventional:

Single sequencing technology

Specific goal: genetic variants

Sequencing gaps

Lower cost

__Many t__  __ypes of genomic analyses require a strong assembly:__

Variant calling, gene expression, regulatory elements

![](img/genomics_31.png)

https://www.ncbi.nlm.nih.gov

https://www.ebi.ac.uk/interpro/entry/pfam/#table

![](img/genomics_32.png)

![](img/genomics_33.png)

https://www.ensembl.org/index.html

https://data.faang.org/home

![](img/genomics_34.png)

![](img/genomics_35.png)

https://www.ebi.ac.uk

![](img/genomics_36.png)

https://rgd.mcw.edu