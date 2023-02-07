---
title: SSH
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 310

menu:
  rivanna-introduction:
    parent: Connecting to Rivanna
---

Finally, we can connect through the _command line_ with `ssh`, the Secure SHell.

* Open OnDemand
   * From the Dashboard, select Cluster, then Rivanna Shell Access. This should log you in directly to a terminal.

## Linux and Mac OS
   * Use a command line
      * `ssh mst3k@rivanna.hpc.virginia.edu`
   * Substitute your own user id for `mst3k`
   * We recommend installing [XQuartz](https://www.xquartz.org/) on Mac OS.

If XQuartz is installed, or you are using Linux, the command should be
```bash
ssh -Y mst3k@rivanna.hpc.virginia.edu
```

## Windows
   * We recommend [MobaXterm](https://mobaxterm.mobatek.net/)
   * Choose an SSH session
       * Enter your user ID and optionally your password
       * Save the password if you wish
