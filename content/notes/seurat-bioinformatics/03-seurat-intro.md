---
title: Intro to Seurat
date: 2025-07-16-17:52:00Z
type: docs 
weight: 200
menu: 
    seurat-bioinformatics:
---

Seurat is an R package that takes the output from Cell Ranger and other pipelines for QC, analysis, and visualization of single cell data. 

**Capabilities:**
  * Basics: preprocessing, QC, dimensional reduction, clustering, differential expression
  * Multimodal analysis (e.g., paired scRNA-seq & scATAC-seq)
  * Spatial datasets (transcriptomic or multiplexed imaging-based)
  * Data integration (multiple sources of single cell data)

[Read More](https://satijalab.org/seurat/)


In this tutorial, we will use Seurat to analyze a dataset of 2,700 Peripheral Blood Mononuclear Cells (PBMCs) made publicly available by 10X Genomics.

The standard unsupervised clustering workflow includes QC and data filtration, calculation of high-variance genes, dimensional reduction, graph-based clustering, and the identification of cluster markers.

