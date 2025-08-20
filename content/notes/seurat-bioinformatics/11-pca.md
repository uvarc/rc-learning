---
title: Interactive Workshop - Linear Dimension Reduction (PCA)
date: 2025-07-16-17:52:00Z
type: docs 
weight: 650
menu: 
    seurat-bioinformatics:
---

PCA (Principal Components Analysis) reduces the number of variables (here, genes) while preserving as much information as possible. The first principal component accounts for the __largest possible variance__ in the dataset. 

Read more: [https://builtin.com/data-science/step-step-explanation-principal-component-analysis](https://builtin.com/data-science/step-step-explanation-principal-component-analysis)

PCA is performed on the scaled data. By default, only the previously determined variable features are used as input (top 2000). The input can be defined using the `features` argument if you wish to choose a different subset. 

```r
pbmc <- RunPCA(pbmc, features = VariableFeatures(object = pbmc))
```

Running PCA gives you PCs (PC scores) and loadings (the weight/importance of each gene for each PC).

