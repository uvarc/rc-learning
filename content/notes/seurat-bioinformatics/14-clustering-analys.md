---
title: Interactive Workshop - Clustering Analysis
date: 2025-07-16-17:52:00Z
type: docs 
weight: 800
menu: 
    seurat-bioinformatics:
---

Clustering helps you to zoom in on your scRNA-Seq data like a microscope and find interesting observations through noise. 

The "distance" metric (distance in the selected PC space) drives clustering analysis based on previously identified PCs. 

First, Seurat embeds cells in a graph structure (K-nearest neighbors, KNN). Then, edges are drawn between cells with similar feature expression patterns and the graph is partitioned into highly interconnected "communities" or "quasi-cliques."

The `FindNeighbors()` function constructs a KNN graph based on Euclidean distance in PCA space. It refines edge weights between any two cells based on shared overlap in local neighborhoods (Jaccard similarity). The input is the previously defined dimensionality of the dataset (first 10 PCs in our example). The `dims` parameter is the number of PCs you determined to be significant based on elbow plot and JackStraw plot. 

The `FindClusters()` function applies modularity optimization techniques (Louvain algorithm by default) to iteratively group cells together. The `resolution` parameter is a set granularity of downstream clustering (higher resolution = more clusters). 0.4-1.2 is typical for datasets of ~3,000 cells. 


Read More: [https://blog.bioturing.com/2022/02/15/the-essence-of-scrna-seq-clustering/](https://blog.bioturing.com/2022/02/15/the-essence-of-scrna-seq-clustering/)

```r
pbmc <- FindNeighbors(pbmc, dims = 1:10)
pbmc <- FindClusters(pbmc, resolution = 0.5)
```

{{< figure src=/notes/seurat-bioinformatics/img/seurat_workshop_20230407_32.png >}}




