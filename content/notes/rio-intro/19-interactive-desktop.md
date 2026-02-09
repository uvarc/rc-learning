---
title: Interactive Desktop
date: 2025-11-12-03:53:56Z
type: docs 
weight: 950
menu: 
    rio-intro:
        parent: Open OnDemand 
---

The Interactive Desktop application launches a virtual desktop session.

The Desktop app is a great tool for managing files and launching applications with a graphical user interface.

A Firefox browser is available and can be used to browse the web. The browser must be launched from a terminal with the network proxy set. Use the following commands in a terminal:
```bash
export HTTPS_PROXY=http://figgis-s.hpc.virginia.edu:8080

export HTTP_PROXY=http://figgis-s.hpc.virginia.edu:8080

firefox
```