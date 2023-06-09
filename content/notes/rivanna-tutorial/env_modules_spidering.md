---
title: Module Hierarchies
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 650

menu:
  rivanna-tutorial:
    parent: The Cluster Environment
---

The module system is _hierarchical_.  It may be necessary to load other modules in order to load a package.  This can occur when different packages are created with different _toolchains_.  The hierarchy prevents incompatible versions of libraries and applications from conflicting.

### Example: R

{{< code file="/notes/rivanna-tutorial/snippets/module_spider_example.txt" lang="plaintext" >}}
