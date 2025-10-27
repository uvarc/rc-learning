---
title: Paths in Linux
date: "2022-10-01T00:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 410

menu:
  hpc-intro:
    parent: Working with Files
---

UVA HPC runs the Linux operating system.  File paths start from _root_, denoted with a forward slash (`/`).  The layout of the folders/directories is like an upside-down tree.

{{< figure src="/notes/hpc-intro/img/unix_tree.png" caption="Schematic of folders on Rivanna. Only some files and folders shown." alt="Diagram of a file system starting at the root (forward slash), branching into 'home', 'bin', 'usr', and 'scratch'. 'Home' and 'scratch' contain user directories; 'home' contains files and a 'Desktop' directory. 'bin' lists commands like 'ls', 'pwd', and 'cd'. 'usr/bin' includes 'bash' and 'zip'." >}}
