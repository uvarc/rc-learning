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

{{< figure src="/notes/hpc-intro/img/unix_tree_white_background.png" caption="Schematic of folders on the HPC system. Only some files and folders shown." alt="Diagram of a file system, represented as an upside-down tree structure. See the description below the 'File System Layout' heading." >}}

## File System Layout
From the root, the file system branches into `home`, `bin`, `usr`, and `scratch`. `home` and `scratch` contain directories named after computing IDs. `bin` contains commands like `ls`, `pwd`, and `cd`. `usr/bin` includes `bash` and `zip`.
