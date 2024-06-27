---
title: Running Executables
date: 2023-12-11T21:14:11-14:00
type: docs 
weight: 1400
menu: 
    rivanna-command-line:
---

Executables are often also called _binaries_. The terms are synonymous in most cases.

The shell has a predefined _search path_. It will look in a sequence of directories for an executable with the specified name, and will invoke the first one it encounters.  If the executable is in this search path, you can simply type its name at the prompt.

```bash
$gedit hello_world.sh
```

here `gedit` is the name of the executable.  Its actual location is /usr/bin/gedit, but /usr/bin is in the default search path.

If the location of the binary is not in your search path, you must type the path to the executable (it can be absolute or relative)

```bash
$./hello_world.sh
```

For security reasons the current directory is not in your default search path. Hence you must explicitly provide the `./` path to execute a binary in the current directory.  If you wish to add it, type (for bash)
```bash
$export PATH=$PATH:.
```
`PATH` is called an _environment variable_. It holds the list of paths to be searched by the shell.  In this example it is essential to add the first `$PATH` or you will lose the default path set by the system.

If you are unsure of the path to the binary you wish to run, the `which` command will tell you the path to the binary the shell will start.
```bash
$which g++
/apps/software/standard/core/gcc/11.4.0/bin/g++
```
