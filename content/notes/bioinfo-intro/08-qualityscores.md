---
title: Quality Scores
date: 2025-08-23-03:19:53Z
type: docs 
weight: 450
menu: 
    bioinfo-intro:
        parent: Bioinformatics
---

`Q` (Quality) scores are defined as a property that is logarithmically related to the base calling error probabilities (`P`). 

### Calculating Phred Quality Scores - Base calling accuracy

$$
Q = -10 \log_{10} P
$$

`Q` represents the sequencing quality score of a given base Q

`P` represents the probability of base call being wrong

{{< figure src=/notes/bioinfo-intro/img/Intro-Bioinformatics-for-posting_20250604_21.png caption="Table source: https://www.illumina.com/Documents/products/technotes/technote_Q-Scores.pdf " width=90% height=90% >}}

While next-generation sequencing metrics vary from those of Sanger sequencing (e.g., no electropherogram peak heights), the process of generating a Phred quality scoring scheme is largely the same. 

[More on Quality Scores](https://help.basespace.illumina.com/files-used-by-basespace/quality-scores)
