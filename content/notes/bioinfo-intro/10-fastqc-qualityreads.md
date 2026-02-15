---
title: Checking Read Quality - FASTQC
date: 2025-08-23T03:19:53Z
date: 2025-08-23-03:19:53Z
type: docs 
weight: 550
menu: 
    bioinfo-intro:
        parent: Bioinformatics
---

FASTQC provides an overview of sequencing read quality.

Sample FASTQC reports displaying varying metrics: 

{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_24.png width=85% height=85% caption="FASTQC report showing per-base sequence quality, with most bases maintaining high quality across reads and slight drops at the read ends typical of Illumina data." >}}

{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_25.png width=85% height=85% caption="FASTQC report showing per-base sequence quality, where read quality declines toward the end, indicating potential sequencing degradation or lower confidence in base calls at later positions." >}}

{{< figure src=/notes/bioinfo-intro/img/readq3.png width=85% height=85% caption="FASTQC report showing a decline in per-base sequence quality toward the end of reads, indicating significant quality drop-off and potential sequencing errors in later positions." >}}

[Read More](https://www.bioinformatics.babraham.ac.uk/projects/fastqc/)
