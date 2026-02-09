---
title: Accessing from SSH
date: 2025-11-12-03:53:56Z
type: docs 
weight: 550
menu: 
    rio-intro:
        parent: Connecting to the System
---

Steps:

1. Start the HSVPN

2. Open your SSH client and type: 
```bash
ssh mst3k@10.xxx.xxx.xxx
```
`mst3k` is replaced with your user ID and the `x`'s are replaced with your VM's IP address (given in the Services app).

{{< figure src=/notes/rio-intro/img/rio-intro_5.png alt="Screenshot showing a terminal window in MobaXterm with the command `ssh mst3k@10.xxx.xxx.xxx` entered. The command line interface displays a green and yellow bar with the date, time, and file path /home/mobaxterm." width=90% height=90% >}}

When prompted for a password, use your EServices password.

This login method provides only command line access to the VM. No graphical user interfaces (GUIs) are usable.



