---
title: Launching an Interactive Session
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 230

menu:
  hpc-intro:
    parent: Interactive Apps with Open OnDemand
---

When you submit a request for an interactive app, it will be placed into the partition you specified, where it will wait until resources become available.  Requests with higher resource requests (more cores, more memory, more time) may wait longer.  

Once the job session begins, the screen will ask you to connect.  In our example you will see a `Connect to Jupyter` button appear.

{{< figure src="/notes/hpc-intro/img/OOD_Jupyter_connect.png" caption="Connecting to a session." >}}

When you connect, you will see your files on the left sidebar and a collection of kernels from which to choose.  You may not see all of these "tiles" because some accounts have customized tiles set up.  

{{< figure src="/notes/hpc-intro/img/OOD_Jupyter_kernels.png" caption="Start screen for JupyterLab" >}}

If you have connected previously, it may start from your earlier status and you will not see the tiles.  

