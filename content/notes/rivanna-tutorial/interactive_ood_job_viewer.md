---
title: The OOD Job Viewer
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 680

menu:
  rivanna-tutorial:
    parent: Interactive Apps with Open OnDemand
---

Open OnDemand allows you to check the status of your jobs easily.  Open the Jobs tab and go to Active Jobs.  The default view is All Jobs.

{{< figure src="/notes/rivanna-tutorial/img/OOD_squeue_viewer.png" caption="Job status viewer in OOD." >}}

You can filter to select subsets of the jobs, for example you can view only jobs in the `gpu` partition.

{{< figure src="/notes/rivanna-tutorial/img/OOD_squeue_filter.png" caption="Viewing only the GPU partition." >}}

You can also look at the status of only your own jobs by switching from All Jobs to My Jobs.

{{< figure src="/notes/rivanna-tutorial/img/OOD_squeue_myjobs.png" caption="Viewing only my jobs." >}}
