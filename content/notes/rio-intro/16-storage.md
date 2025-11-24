---
title: Storing Data
date: 2025-11-12-03:53:56Z
type: docs 
weight: 800
menu: 
    rio-intro:
---

Virtual machines do not come with any significant disk storage of their own. 

A PI specifies the storage space they would like to have when requesting access to Ivy. 
  
The default allocation is of 1TB of Research Standard Storage.
  
This storage is mounted under  `/data/ivy-hip-name`, where `ivy-hip-name` is replaced by the name of your Ivy project's Grouper group name.
  
Storage can be resized upon request. Submit your request [here on the RC website](https://www.rc.virginia.edu/form/storage/).

JupyterLab, RStudio, Rio, and FastX can all save data into the shared storage at `/data/ivy-hip-name`.


