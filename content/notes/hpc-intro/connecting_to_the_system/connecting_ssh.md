---
title: Connecting with SSH (Terminal)
date: "2022-10-01T00:00:00"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 180

menu:
  hpc-intro:
    parent: Connecting to The System
---

SSH client provides direct access to the command line.
Use the command 
```
ssh -Y mst3k@login.hpc.virginia.edu```
- On a Mac or Linux system, simply open a terminal application and type the above command.
- On a Windows system, open the Command Prompt app and type the above command.
- Replace the `mst3k` with your computing ID.
- You must use the UVA VPN when off-grounds.
FastX is a Web-based desktop environment for HPC. It is accessible either through the [Open OnDemand Interactive Apps](https://ood.hpc.virginia.edu/pun/sys/dashboard) menu, or directly at [fastx.hpc.virginia.edu](https://fastx.hpc.virginia.edu).

FastX requires the VPN.  If the VPN is not active, the start page _will not_ load.

Always use either the OOD link or the `fastx` URL.  The underlying name of the host may change from time to time.

{{< figure src="/notes/hpc-intro/img/FastX_splash_screen.png" caption="Logging in to FastX" >}}

