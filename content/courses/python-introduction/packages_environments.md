---
title: Managing Python Packages and Environments
toc: true
type: docs
draft: false
weight: 12
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
        parent: Introduction to Programming in Python
---

## Managing Packages with Conda and Mamba

Conda is a package manager from the developers of [Anaconda](anaconda.com).  It is free, but if you use a conda supplied by Anaconda, please pay attention to their licensing terms.  We recommend Miniforge, which provides its own build of conda, as well as an alternative called Mamba.  Mamba is for most purposes a drop-in replacement for conda.  It is generally faster than conda.  Miniforge will draw packages from the `conda-forge` channel, which is a collection of community-built and maintained packages that are free for use.

On the UVA HPC system, please use `conda` rather than `mamba` to create environments.  Once an environment has been created, `mamba` can be used to install packages.  On a personal system, either `conda` or `mamba` may be used to create environments.

Conda or mamba can be used from a command line.  In Linux and Mac OS, the terminal can be used for this.  In Windows, use the Miniforge Prompt, which can be accessed through the Apps menu in the Miniforge folder, since it has the correct paths preset.  

To update a package, type
```python
mamba update package
```

You can also install packages with the mamba or conda command line.
```python
mamba install newpackage
```
Many more options are available.  

## Environments

When you use conda or mamba, you always have an _environment_; the one with which you start is called _base_.  An environment is a "bundle" of a Python version, which need not be the same as your base, along with a set of packages installed against that version.  Only one environment can be active at a time (for Linux users, in a given shell) but environments can be activated and deactivated at will.  

A common use for conda/mamba environments is to create "sandboxes" to avoid duplication or to use different versions of Python.  For example, Xarray is a general-purpose package but it is particularly aimed at geophysical applications.  Suppose you wish to use it only for a particular application (a homework problem, for example) but do not wish to install it into your main environment.  To create a new environment for your Xarray project, start the terminal or Miniforge prompt.

From the command line, run
```bash
conda create --name geodat --python=3.11
```
or
```bash
mamba create --name geodat --python=3.11
```
Python must be explicitly included and its version must be specified.  To switch to the new environment, from the command line run 
```bash
conda activate geodat
```
or
```bash
mamba activate geodat
```

On the UVA HPC system, please use
```bash
source activate geodat
```

Once activated, run
```
mamba install xarray
```
along with other packages you may wish to include.  If you wish to use Jupyterlab and/or Spyder, you must install those as well.

Conda's [user guide](https://conda.io/projects/conda/en/latest/user-guide/index.html) describes conda's capabilities.

The Conda [cheat sheet](https://docs.conda.io/projects/conda/en/4.6.0/_downloads/52a95608c49671267e40c689e0bc00ca/conda-cheatsheet.pdf) is handy.

Documentation for Mamba is [here](https://mamba.readthedocs.io/en/latest/) In the Mamba documentation, "CLI" stands for "command-line interface" and "CI" stands for "continuous integration," which is an approach to software development.)


Keep in mind that each environment basically copies files into distinct folders, so creating many of them can consume disk storage space rapidly.

### Pip

Pip is different package installer not connected directly to conda or mamba.  It can be used in any conda environments, but conda/mamba will not be aware of packages installed with pip.  However, not all packages or versions are available through conda so pip is useful.  It is command-line based. Miniforge provides a pip executable for cases where conda/mamba cannot be used or the user would prefer pip.  

For most packages it is sufficient to type
```bash
pip install pkg
```
Another option is
```bash
python -m pip install pkg
```
The latter form ensures that the pip is compatible with the Python in use.

As is the case for conda or mamba, the exact name of the package must be known.

Documentation for pip is at the official [site](https://pip.pypa.io/en/stable/getting-started/).

