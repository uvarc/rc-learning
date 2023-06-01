---
title: Using Modules
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 440

menu:
  rivanna-tutorial:
    parent: The Cluster Environment
---

Find all available versions of MATLAB.  You may see a different list.

{{< code file="/notes/rivanna-tutorial/snippets/module_example.txt" lang="no-highlight" >}}

Load a specific version of MATLAB
```bash
module load matlab/R2022a
```

Change versions
```bash
module swap matlab/R2022a matlab/R022b
```

Clean up
```bash
module purge
```
