---
title: JupyterLab Terminal
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 250

menu:
  hpc-intro:
    parent: Interactive Apps with Open OnDemand
---

You are also able to access the terminal through a JupyterLab session.

{{< figure src="/notes/hpc-intro/img/OOD_JupyterLab_Terminal.png" caption="JupyterLab Terminal" >}}

Here, you can execute Linux commands to create custom conda environments and JupyterLab kernels. Additionally, you can access and run singularity containers through this functionality.

Your JupyterLab sessions will be saved in your ```/home/computingID/ondemand/data/sys/dashboard/batch_connect/sys/jupyter_lab/output/``` directory; however, you can navigate to any part of the filesystem in the JupyterLab terminal.
