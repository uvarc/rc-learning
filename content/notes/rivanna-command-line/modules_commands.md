---
title: Modules Commands
date: 2023-12-11T21:14:11-14:00
type: docs 
weight: 2600
menu: 
    rivanna-command-line:
---

{{< table >}}
|  Command   |  Result  |
|  -------   |  ------  |
|  module load <name> |  load the default module for the <name> package |
|  module load <name>/<version> | load the specific version of a package |
|  module spider <name> | view the available versions of a package | 
|  module spider <name>/<version> | view instructions for loading a version |
|  module purge | clear all modules |
|  module swap <name>/<version1> <name>/<version2> | switch from version 1 to version 2 |
{{< /table >}}

**Examples**
```bash
$module load gcc
$module load matlab/R2023a
$module spider R/4.3.1
$module load gcc/11.4.0 openmpi/4.1.4 R/4.3.1
$module purge
```

