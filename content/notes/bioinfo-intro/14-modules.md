---
title: Modules and Commands
date: 2025-08-23T03:19:53Z
type: docs 
weight: 750
menu: 
    bioinfo-intro:
        parent: Research Computing Resources
---

Modules are used to manage software packages.

**Helpful Commands:**

`module avail` lists all available modules (there may be a lot!). 

`module spider <package>` lists all available versions of `<package>`. 

`module spider <package>/<version>` describes how to load `<package>/<version>`. There may be prerequisite modules. 

`module key <search term>` provides a handy keyword search of all modules categorized into "application classes". 

`module list` lists modules loaded in the current shell. 

`module purge` removes all module modifications in your environment.

`module load <package>/[<version>]` loads the module for (optionally) `<version>` of `<package>`. 

`module unload <package>` deletes the changes made by the `<package>` module. 

`module swap <package>/<current> <package>/<newver>` exchanges one version of a package for another. 

[Learn More](https://learning.rc.virginia.edu/notes/hpc-from-terminal/section5/18)




