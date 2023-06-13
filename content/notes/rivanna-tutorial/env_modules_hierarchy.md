---
title: Selecting A Module Version
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 652

menu:
  rivanna-tutorial:
    parent: The Cluster Environment
---

After listing the available versions, we decide we want to use R version 4.0.3

{{< code file="/notes/rivanna-tutorial/snippets/module_hierarchy_example.txt" lang="plaintext" >}}

{{< info >}}
Note the line "You will need to load all module(s) on any one of the lines below..."  You should load _either_ one set _or_ the other, not both.
{{< /info >}}

```bash
module load gcc/7.1.0
module load openmpi/3.1.4
module load R/4.0.3
```



