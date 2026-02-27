---
title: VM Storage
date: 2025-11-12T03:53:56Z
type: docs 
weight: 653
menu: 
    rio-intro:
---

Storage is split across two sets of directories. 

1. Personal directory 

The personal directory for individual data storage is mounted under `/home/$USER`. Little space is allotted for `/home` directories per VM. Individual home folders are personal, and not shareable. 

2. Shared storage space 

Shared storage space is mounted under `/standard/ivy-hip-name` where `ivy-hip-name` is replaced by the name of your Ivy project's Grouper group name. 

The default of Research Standard Storage is 1TB. PIs can get more when first requesting the Ivy project. Storage can be resized using our [Storage Request Form](https://www.rc.virginia.edu/form/storage/ "The RC website's Storage Request form").

### Rio Caveat 

One caveat to consider when working in the Rio environment is that `/home` is not mounted on Rio compute nodes. This means that if your compute jobs reference any files in your `/home` storage, they will not be found. Ivy VMs not using Rio can ignore this condition. 

We recommend working exclusively out of the VM's Research Standard storage directly for Rio. Individual user subdirectories are automatically created under `/standard` to organize the space. 