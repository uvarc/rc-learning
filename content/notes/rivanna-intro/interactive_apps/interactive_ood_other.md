---
title: Other Interactive Apps
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 280

menu:
  rivanna-intro:
    parent: Interactive Apps with Open OnDemand
---

Some other widely-used interactive apps are MATLAB and the Desktop.  

## Desktop

The most general OOD interactive app is the Desktop.  It will start a desktop identical to the FastX desktop, but on a compute node rather than a frontend.  From the desktop you can open a variety of applications from the menu, such as the Caja file manager.  You can also open a terminal window and type any valid commands into it.  In this illustration, the user has loaded a module to build a program for running. The OOD interactive desktop is the preferred method for interactive jobs.

{{< figure src="/notes/rivanna-intro/img/OOD_Desktop_terminal.png" caption="The OOD Desktop." >}}

## MATLAB

The MATLAB interactive app starts a MATLAB Desktop environment on a Desktop (VNC). Similar to Rstudio Server, in the Webform you can choose a version of MATLAB from a dropdown menu.  Once there, you are in a less complete desktop environment.  Your files may not be visible on the Desktop, but you can access them from the Places menu or from the Caja (filing cabinet) icon in the ribbon at the top of the screen.  If you exit the MATLAB Desktop, it will also exit the session.

## Reconnecting

Both MATLAB and the Desktop will persist if your network is disconnected. Log back in to Open OnDemand, find your session from the My Interactive Sessions tab, then click `Launch` again.
