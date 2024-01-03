---
title: Checking your Allocation
date: 2023-12-11-14:11:14Z
type: docs 
weight: 1800
menu: 
    rivanna-command-line:
---

To see how many SUs you have available for running jobs, type at the command-line prompt `allocations`.

```bash
$allocations
 Allocations available to Misty Tea (mst3k):
  ds5559: less than 25,000 service-units remaining
  ga_bioinfo-test: less than 100,000 service-units remaining
```

For more information about a specific allocation, please run
```bash
$allocations -a <allocation name>
```

