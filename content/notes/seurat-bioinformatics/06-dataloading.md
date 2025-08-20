---
title: Interactive Workshop - Data Access & Loading
date: 2025-07-16-17:52:00Z
type: docs 
weight: 450
menu: 
    seurat-bioinformatics:
---

Note: The rest of this interactive tutorial uses the PBMC 3k dataset as an example. You can either download it yourself from 10x Genomics or use the built-in copy available via the `SeuratData` package.

The below script loads the data using `SeuratData`. 

```r
# Loading the data
install.packages("SeuratData")
library(SeuratData)
InstallData("pbmc3k")
data("pbmc3k")
pbmc.data <- pbmc3k

nrow(pbmc.data)

print(pbmc.data[1,1:5])

# Initialize the Seurat object with the raw (non-normalized) data
pbmc <- CreateSeuratObject(counts = pbmc.data, project = "pbmc3k", min.cells = 3, min.features = 200)

pbmc
```
