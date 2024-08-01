---
title: Using and Closing an Interactive Session
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 240

menu:
  hpc-intro:
    parent: Interactive Apps with Open OnDemand
---

If have not previously used the OOD JupyterLab interactive app, you must select a kernel before initiating the notebook.  Once JupyterLab is set up, you can also start another notebook with a different kernel by selecting File->New Notebook.  It will then show a dropdown with the kernels available to you.

{{< figure src="/notes/hpc-intro/img/OOD_Jupyter_nb.png" caption="Starting a new notebook." >}}

If you are accidentally disconnected, you can go back to the OOD "My Interactive Sessions" tab and reconnect.  However, anything left running in a cell may have been terminated.  This is due to a limitation of Jupyter, not OOD or the HPC cluster, and does not apply to all interactive apps.

Remember to delete your session if you finish early. Closing your browser tab does not end the session.

{{< figure src="/notes/hpc-intro/img/OOD_delete_session.png" caption="Ending a session." >}}

