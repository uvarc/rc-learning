---
title: Filter & Normalize
date: 2025-07-16-17:52:00Z
type: docs 
weight: 550
menu: 
    seurat-bioinformatics:
     parent: Interactive Workshop
---

### Filter the Data

Visualized QC metrics are then used to filter cells.  

Remove cells with unique features (genes) over 2,500 or less than 200, and remove cells that have >5 % mitochondrial counts. 

Note that `subset()` arguments define cells you want to _keep_ (e.g., `percent.mt` < 5).

```r
# Subset cells for further analysis based on QC metrics. 
pbmc <- subset(pbmc, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)
```
### Normalize the Data 

Use global-scaling normalization (`LogNormalize`). 

For each cell, (a) divide feature expression counts by the total counts for that cell, (b) multiply by a scale factor (default is 10,000), and (c) apply a log transformation.

The normalized data is stored in `pbmc[["RNA"]]@data`. 

```r
pbmc <- NormalizeData(pbmc, normalization.method = "LogNormalize", scale.factor = 10000)
```
