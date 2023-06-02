---
title: Software Modules
date: "2022-10-01:00:00Z"
draft: false  # Is this a draft? true/false
toc: false  # Show table of contents? true/false
type: docs  # Do not modify.
weight: 430

menu:
  rivanna-tutorial:
    parent: The Cluster Environment
---

Many application software packages are installed on Rivanna, frequently in multiple versions.  Software _modules_ make it much easier for users to access the packages and versions they want to use.  A _module_ is a script provided by the system that sets up paths and any environment variables that may be needed in order to run the program(s).

## Module Commands

These must be typed into a terminal. The commands must be typed into each terminal window, before any other commands to use the software.

* List all available modules and versions.
```bash
module avail
``` 
* Show all available modules
```bash
module spider
``` 
* Show available modules for a specific package
```bash
module spider package
```
* Show modules with the <keyword> in the description
```bash
module key <keyword>  
```
* Load the module to set up the environment for the default version of some software.
```bash
module load <name>
```

Unless otherwise noted, the default version of a package will be that with the _highest_ number.  If a different version is default, a `(D)` will be printed next to its name.

* Load a specific version `N.m` of software `mypackage`.
```bash
module load package/N.m
```
* Swap one version for another
```bash
module swap mypackage/N.m mypackage/K.l
```
* List modules currently loaded in your environment.
```bash
module list 
```
* Clear all modules
```bash
module purge
```
