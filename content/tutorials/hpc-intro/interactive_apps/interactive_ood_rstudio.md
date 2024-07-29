---
date: "2022-10-01"
title: "Rstudio Server"
weight: 260
---

Rstudio Server is a standalone app like JupyterLab. Starting a session is very similar to JupyterLab, but the form differs slightly.  Instead of kernel tiles, you will select a version of R from a dropdown menu from those available.  In this example, the version is R 4.2.2.

{{< figure src="/tutorials/hpc-intro/img/OOD_Rstudio_form.png" caption="Starting an Rstudio session." >}}

Rstudio Server can continue running any active processes if your network is disconnected.  Simply log back in to Open OnDemand, go to the "My Interactive Sessions" tab, and click `Launch` again.  It will reconnect, not launch another session.  
