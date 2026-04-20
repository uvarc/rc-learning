---
title: Modules Basics
date: 2026-04-20T02:29:07Z
type: docs 
weight: 1300
menu: 
    rio-intro:
        parent: Preinstalled Software
---

Some basic commands to use lmod:

- `module avail` - lists every module available to load
- `module key <keyword>` - searches for modules in a category specified (e.g., `module key bio`)
- `module spider <module-name>` - prints information about the software including different offered versions
- `module load <module-name>` - load the desired software (use `module load <module-name/ver-number>` to load a specific version)
- `module unload <module-name>` - unloads the desired software
- `module list` - prints all currently loaded modules
- `module purge` - unloads all modules


