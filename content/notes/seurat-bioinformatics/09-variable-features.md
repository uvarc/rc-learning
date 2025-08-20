---
title: Interactive Workshop - Identify Highly Variable Features
date: 2025-07-16-17:52:00Z
type: docs 
weight: 600
menu: 
    seurat-bioinformatics:
---

Next, identify the subset of features that exhibit high cell-to-cell variation. In other words, they are highly expressed in some cells, and lowly expressed in others. 

Focusing on these genes in downstream analysis helps to highlight biological signal in single-cell datasets. 

This step directly models the mean-variance relationship inherent in single-cell data, where genes with higher mean expression tend to have higher variance across cells. The `FindVariableFeatures()` function returns 2,000 features per dataset by default. 

Use these variable features in downstream analysis, such as PCA.

```r
pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)

# Identify the 10 most highly variable genes
top10 <- head(variableFeatures(pbmc), 10)

# Plot variable features with labels 
plot1 <- variableFeaturePlot(pbmc)
plot1 <- LabelPoints(plot = plot1, points = top10, repel = TRUE)
plot1
```

Output:

{{< figure src=/notes/seurat-bioinformatics/img/seurat_workshop_20230407_15.png >}}

We measure average expression against standardized variance to identify genes that are more variable than expected given their mean expression. Standardizing the variance removes the natural relationship where genes with higher mean expression also tend to have higher variance.

In the plot, the PPBP (Pro-Platelet Basic Protein) gene is highly variable because it is expressed only in platelets.

