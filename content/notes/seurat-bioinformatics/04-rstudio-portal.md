---
title: OOD RStudio Server
date: 2025-07-16-17:52:00Z
type: docs 
weight: 250
menu: 
    seurat-bioinformatics:
      parent: Interactive Workshop
---

Access the RStudio Server on UVA Open OnDemand.

1. Log in with your UVA credentials. 

2. From the Interactive Apps menu, select RStudio Server (see below).

{{< figure src=/notes/seurat-bioinformatics/img/seurat_workshop_20230407_5.png width=70% height=70% >}}

3. Fill the job parameters and hit Launch (see below).

{{< figure src=/notes/seurat-bioinformatics/img/seurat_workshop_20230407_6.png width=50% height=50% >}}

<small> Note: leave the Slurm Option empty, and use the 'Standard' partition. The parameters in the image are specific to the live workshop demonstration. </small>

Let’s take a quick look at some of the fields:
  * Rversion: Rstudio 1.3.1073 – R 4.0.2
  * Rivanna Partition: Standard
  * Number of hours: 3 hours
  * Number of cores: 1
  * Memory Request in GB ( maximum 384G ): 10G
  * Allocations (SUs): rivanna-training `# replace this with your allocation`
  * Launch

