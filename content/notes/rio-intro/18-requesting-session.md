---
title: Requesting a Session
date: 2025-11-12T03:53:56Z
type: docs 
weight: 900
menu: 
    rio-intro:
        parent: Open OnDemand 
---

To request an interactive session, select one of the applications from the dropdown. This will prompt you to fill in the desired resources for your interactive job. 

{{< figure src=/notes/rio-intro/img/jupyterlab.png alt="Open OnDemand JupyterLab launch form showing 6 fields: Partition set to Standard, 1 hour, 1 core, 6 GB memory, allocation hpc_build, Show Additional Options set to No, and a Launch button." width=45% height=45% >}}

The above example is for starting a JupyterLab session.

### Choosing Resource Requests

**Partition:** Standard or GPU (only select GPU if you are using GPU-enabled code). 

**Time:** Time limit for your session. Once reached, your session will terminate along with any running code. This cannot be adjusted once set. 

**Cores:** Used in parallel processing. Your code must be modified to take advantage of using multiple cores. 

**Memory:** A good rule of thumb is to request 2 to 3 times the size of the data that you are reading in or generating. 

**Optional Slurm Option:** This field is used to input one of the many other Slurm options. 

### Queueing the Session

Once you've filled out the resource request form, click the Launch button at the bottom to queue your job. While queued, the job will wait for the resources you've asked for to become available. Requests with higher resource requests (more cores, more memory, more time) may wait longer. 

{{< figure src=/notes/rio-intro/img/queue.png alt="Open OnDemand interface showing a queued JupyterLab session with creation time, one-hour request, session ID link, and a Delete button." caption="Example of a JupyterLab session waiting in the queue on Open OnDemand." width=90% height=90% >}}

### Launching the Session

When the job has started, the status will change from Queued to Running. A button will appear to open a browser, launching the interactive session.

{{< figure src=/notes/rio-intro/img/runningjob.png alt="Open OnDemand interface showing a running JupyterLab session. The display includes the host node name, creation time, remaining time, session ID link, a Delete button, and a blue 'Connect to Jupyter' button." caption="Example of a JupyterLab session that has started running and is ready to connect in Open OnDemand." width=90% height=90% >}}
