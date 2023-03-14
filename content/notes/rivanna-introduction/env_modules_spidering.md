---
title: Module Hierarchies
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 450

menu:
  rivanna-introduction:
    parent: The Cluster Environment
---

The module system is _hierarchical_.  It may be necessary to load other modules in order to load a package.  This can occur when different packages are created with different _toolchains_.  The hierarchy prevents incompatible versions of libraries and applications from conflicting.

### Example: R

```bash
module spider R

----------------------------------------------------------------------------
  R:
----------------------------------------------------------------------------
    Description:
      R is a free software environment for statistical computing and
      graphics.

     Versions:
        R/3.5.3
        R/3.6.3
        R/4.0.3
        R/4.1.1
        R/4.2.1
     Other possible modules matches:
        agrep  amber  amptorch  apr  apr-util  archspec  aria2  armadillo  ...

----------------------------------------------------------------------------
  To find other possible module matches execute:

      $ module -r spider '.*R.*'

----------------------------------------------------------------------------
  For detailed information about a specific "R" package (including how to load t
he modules) use the module's full name.
  Note that names that have a trailing (E) are extensions provided by other modu
les.
  For example:

     $ module spider R/4.2.1
----------------------------------------------------------------------------
```
