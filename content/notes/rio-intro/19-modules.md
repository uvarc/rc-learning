---
title: Modules
date: 2025-11-12-03:53:56Z
type: docs 
weight: 950
menu: 
    rio-intro:
        parent: Preinstalled Software
---

Any application software that you want to use will need to be loaded with the `module load` command.

For example:
```bash
module load apptainer/1.3.1
```
You will need to load the module any time that you create a new shell. This includes every time that you log out and back in as well as every time that you run a batch job on a compute node.

**Module Commands**

Some basic commands to use `lmod`:

`module avail` lists every module avaiable to load.

`module key <keyword>` searches for modules in a specified category (i.e. `module key bio`).

`module spider <module-name>` prints information about the software including different offered versions.

`module load <module-name>` loads the desired software.

`module load <module-name/ver-number>` loads a specific version of a module.

`module unload <module-name>` unloads the desired software.

`module list` prints all currently loaded modules.

`module purge` unloads all modules.
