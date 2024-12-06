---
title: Rstudio Server
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 260

menu:
  hpc-intro:
    parent: Interactive Apps with Open OnDemand
---

Rstudio Server is a standalone app like JupyterLab. Starting a session is very similar to JupyterLab, but the form differs slightly.  Instead of kernel tiles, you will select a version of R from a dropdown menu from those available.  In this example, the version is R 4.4.1.

{{< figure src="/notes/hpc-intro/img/Interactive-RStudio-form-2024.png" caption="Starting an Rstudio session." >}}

Rstudio Server can continue running any active processes if your network is disconnected.  Simply log back in to Open OnDemand, go to the "My Interactive Sessions" tab, and click `Launch` again.  It will reconnect, not launch another session.  
