---
title: Configure
date: 2026-04-14T16:42:59Z
type: docs 
weight: 650
menu: 
    building-running-c-cpp-fortran:
---

The `configure` is a way to generate a Makefile automatically.

We will not discuss creating configure scripts, since they can be quite complex. But much software is distributed with them.  Configure usually takes many options and they vary by package. To see them, run from its directory 

```bash
./configure --help
```

Most configure scripts assume the software will be installed into /usr or /usr/local. You do not have permission to do this on an HPC system, so you will nearly always need to use the `-prefix` option. 

```bash
./configure -prefix=/home/mst3k/udunits
```

Configure should produce a Makefile. 
When it has completed you can run `make` as usual. This is usually followed by

```bash
make install
```

Refer to the documentation for your package for more details.

**Exercise** 

Copy udunits.tar.gz from /share/resources/tutorials/compilers

Untar it (`tar xf udunits.tar.gz`). Cd into its directory. Run

```bash
configure --help
```

After examining the options, run it with at least the prefix option. You may wish to create a new directory for installing the package.

Build the library.

Install the library into the directory prepared for it.

