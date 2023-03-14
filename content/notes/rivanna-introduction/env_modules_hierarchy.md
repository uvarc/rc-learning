---
title: Selecting A Module Version
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 452

menu:
  rivanna-introduction:
    parent: The Cluster Environment
---

After listing the available versions, we decide we want to use R version 4.0.3

```bash
module spider R/4.0.3
----------------------------------------------------------------------------
  R: R/4.0.3
----------------------------------------------------------------------------
    Description:
      R is a free software environment for statistical computing and
      graphics.


    You will need to load all module(s) on any one of the lines below before the
 "R/4.0.3" module is available to load.

      gcc/7.1.0  openmpi/3.1.4
      intel/18.0  intelmpi/18.0
 
    Help:
      
      Description
      ===========
      R is a free software environment for statistical computing and graphics.
      
      
      More information
      ================
       - Homepage: http://www.r-project.org/
      
      
      Included extensions
      ===================
      assertthat-0.2.1, b-a, base64enc-0.1-3, cli-2.0.2, crayon-1.3.4, d-a,
      digest-0.6.25, ellipsis-0.3.0, evaluate-0.14, fansi-0.4.1, float-0.2-4, g-
r,
      g-r, g-r, glue-1.4.0, htmltools-0.4.0, IRdisplay-0.7.0, IRkernel-1.1,
      jsonlite-1.6.1, m-e, pbdZMQ-0.3-3, pillar-1.4.4, Rcpp-1.0.4.6, repr-1.1.0,
      rlang-0.4.6, rlecuyer-0.3-5, s-p, s-t, s-t, t-o, u-t, utf8-1.1.4, uuid-0.1
-4,
      vctrs-0.3.0
```

{{< info >}}
Note the line "You will need to load all module(s) on any one of the lines below..."  You should load _either_ one set _or_ the other, not both.
{{< /info >}}

```bash
module load gcc/7.1.0
module load openmpi/3.1.4
module load R/4.0.3
```



