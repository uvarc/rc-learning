---
date: "2023-05-01"
title: "Containers vs VMs"
weight: 30
---

You may be familiar with the virtual machines (VMs), which accomplish the same goal as containers: to create a reproducible and shareable compute environment. The big difference between VMs and containers is that VMs provide their own guest OS, whereas containers don't require an OS and run on the Docker Engine and share the host's OS kernel.

The size of Docker images is usually on the order of tens of MBs, while VMs can be several tens of GBs large.

<img src="/notes/containers/img/container-vs-vm.pbm" /> 
