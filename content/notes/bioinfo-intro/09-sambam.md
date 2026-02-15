---
title: SAM/BAM Sequence Alignment
date: 2025-08-23T03:19:53Z
type: docs 
weight: 500
menu: 
    bioinfo-intro:
        parent: Bioinformatics
---

An alignment file provides context for raw data. It has 11 tab-delimited columns with one alignment record per line. 

`SAM` is plain-text (human readable), whereas `BAM` is in binary format. 

[SAMTools](http://samtools.sourceforge.net) is a suite of utilities for SAM/BAM files. [Picard](https://broadinstitute.github.io/picard/) is a set of tools for sequencing data. 

### Example SAM file

```plaintext
D4ZHLFP1:53:D2386ACXX:6:2115:17945:68812 0 Mle_000001 18 42 108M * 0 0
    TCCCCCTGCATGGTCCGTCTGCGTGCAATCGCATGAGTATGCCTCCAGCATGAGTTACCGATCGTGGACACCTGCTTG
GCCAAGATGTACTGAGATGCAT
C@CFDEFFHHGHHFGBGFEGGDGGGEHGHGGGJJJJIIGIIB9BFBFHGHHICEAHGGEGEDHIGEEDBECCACBDDC@CCDBCDD<
?2+4>@4>>CCCAA@@  AS:i:-5 XN:i:0 XM:i:1 XO:i:0 XG:i:0 NM:i:1 MD:Z:0A107 
    YT:Z:UU
D4ZHLFP1:53:D2386ACXX:7:2110:5214:83081 0 Mle_000001 18 42 108M * 0 0
TCCCCCTGCATGGTCCGTCTGCGTGCAATCGCATGAGTATGCCTCCAGCATGAGTTACCGATCGTGGCAACCTGCTTGCCAA
GATGTACTGAGATGCAT
CCCFFFFHHHHHHHGGGEGIJIIGJFHJJJJIJIJJIJIJGIJJIJJIJFHJJJIJJHHFFCEEEEEDDDDDDDDDDDDD  AS:i:-5 XN:i:0 XM:i:1 XO:i:0 XG:i:0 NM:i:1 MD:Z:0A107      
    YT:Z:UU
D4ZHLFP1:53:D2386ACXX:7:2206:9985:31556 0 Mle_000001 18 42 108M * 0 0
TCCCCCTGCATGGTCCGTCTGCGTGCAATCGCATGAGTATGCCTCCAGCATGAGTTACCGATCGTGGCAACCTGCTTGCCAA
GATGTACTGAGATGCAT
CCCEFFFFHHHHHJJIJHJJIJIJJIJIJJJJIJIJJJIJJIJJJIJJJGEFFEEEEDDDDDDDDDDDDDDDDDDDDDDD  AS:i:-5 XN:i:0 XM:i:1 XO:i:0 XG:i:0 NM:i:1 MD:Z:0A107 
    YT:Z:UU
```

Helpful site for looking up `SAM` flags: [https://broadinstitute.github.io/picard/explain-flags.html](https://broadinstitute.github.io/picard/explain-flags.html)