---
title: 10X Genomics Pipeline
date: 2025-07-16T17:52:00Z
type: docs 
weight: 150
menu: 
    seurat-bioinformatics:
---

10X Genomics is the company that pioneered scRNA-seq.

The Chromium platform is used for sequencing individual cells.
The resulting data is then processed using the Cell Ranger pipeline.

{{< figure src=/notes/seurat-bioinformatics/img/seurat_workshop_20230407_1.png width=95% height=95% >}}

Cell Ranger Pipeline:

* Reads are first aligned to a reference genome.
* Next, low-quality reads are filtered out.
* Barcodes are then counted to identify individual cells.
* UMIs (unique molecular identifiers) are counted to quantify transcripts.
* Finally, expression matrices are generated for downstream analysis.

[More Information](https://support.10xgenomics.com/single-cell-gene-expression/software/pipelines/latest/what-is-cell-ranger)

UVA RC has Cell Ranger on Rivanna, so you can use it to process your scRNAseq data. This tutorial will not go into details about that, instead focusing on visualization/downstream analysis. 

### 10X Genomics Cell Ranger Outputs

__`barcodes.tsv`__

This file lists 1 barcode per cell.

{{< figure src=/notes/seurat-bioinformatics/img/seurat_workshop_20230407_2.png width=35% height=35% >}}

__`genes.tsv`__

This file contains the **Ensembl** IDs and gene symbols.

{{< figure src=/notes/seurat-bioinformatics/img/seurat_workshop_20230407_3.png width=25% height=25% >}}

__`matrix.mtx`__

This file contains the actual expression data.

{{< figure src=/notes/seurat-bioinformatics/img/seurat_workshop_20230407_4.png width=35% height=35% >}}


