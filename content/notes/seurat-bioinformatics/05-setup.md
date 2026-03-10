---
title: Loading Seurat
date: 2025-07-16T17:52:00Z
type: docs 
weight: 400
menu: 
    seurat-bioinformatics:
        parent: Interactive Workshop
---

Rivanna does not ship Seurat as a prebuild module, but you can access it through installing it into your R library. 

In your R console: 

```r
# Install Seurat and any other packages you need
install.packages(c("Seurat","dplyr","patchwork"))

# Load Seurat
library(Seurat)
library(dplyr)
library(patchwork)

```

### Set Up a Working Directory

```r
# Set working space 
user <- Sys.getenv("USER")
dir.create(file.path("/scratch", user, "my-seurat-project"))
setwd(file.path("/scratch", user, "my-seurat-project"))

# Copy your data and upload/place your dataset in the working data

# move to the data direectory on your scratch space
setwd(file.path("/scratch", user, "my-seurat-project", "data"))
dir()

# Optional: set a project-local library
.libPaths("./R_libs")
.libPaths()

# Load the libraries (repeat if starting a new session)
library(Seurat)
library(dplyr)
library(patchwork)
```