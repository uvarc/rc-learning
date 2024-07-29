---
date: "2022-10-01"
title: "Interactive JupyterLab Sessions"
weight: 220
---

From the Interactive Apps menu, select JupyterLab.

The Jupyter Web Form gathers information about the computing resources that you need to run your Jupyter Notebook.

{{< info >}}
After you fill in the form, it will remember settings the next time that you connect to it, so watch out if you wish to change something.
{{< /info >}}

{{< figure src="/tutorials/hpc-intro/img/OOD_juplab_form.png" caption="Setting up a job in JupyterLab through OOD" >}}

You must choose a partition from the dropdown list. The partition limitations are explained below the dropdown box. Most of the time, you will select the __Standard__ partition.  If you are running a deep learning model, you will want to choose a __GPU__ Partition. If you do not specify a GPU model, your job will be assigned to the first available.

The "Number of hours" is the time your job session will remain active.  

{{< warning >}}
If you exceed the requested time limit, your session will be terminated without warning.
{{< /warning >}}

The "Allocation" is the name of the allocation that should be charged.  Your advisor should have told you what to use.  You can be a member of more than one allocation.  In that case one of them, not chosen by you, will be the default.  It is best to always fill in the name of an allocation, but remember to change it if necessary.

Once you have completed the form, click on the `Launch` button to submit the request.
