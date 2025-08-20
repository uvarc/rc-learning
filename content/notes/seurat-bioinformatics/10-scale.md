---
title: Scale the Data
date: 2025-07-16-17:52:00Z
type: docs 
weight: 625
menu: 
    seurat-bioinformatics:
        parent: Interactive Workshop
---

Linear transformation is the standard pre-processing step prior to dimensional reduction (PCA, etc.)

The `ScaleData()` function shifts the expression of each gene so that the mean expression across cells is 0. It also scales the expression of each gene so that the variance across cells is 1. 

This step gives equal weight in downstream analysis, so that highly-expressed genes do not dominate results. 

The results of this step are stored in `pbmc[["RNA]]@scale.data`.

```r
all.genes <- rownames(bpmc)
pbmc <- ScaleData(pbmc, features =  all.genes)
```
Notes:

`ScaleData()` now incorporates the functionality of the function formerly known as `RegressOut()` (which regressed out given the effects of provided variables and then scaled the residuals). To make use of the regression functionality, simply pass the variables you want to remove to the `vars.to.regress` parameter.

Setting `center = TRUE` will center the expression for each feature by subtracting its average expression. Setting `scale = TRUE` will scale each feature's expression by dividing the centered values by their standard deviation (if `center = TRUE`) or by the root mean square otherwise. 