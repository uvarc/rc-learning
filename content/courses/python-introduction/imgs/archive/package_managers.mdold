---
title: Managing Python Packages and Environments
toc: true
type: docs
draft: false
weight: 14
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
        parent: Introduction to Programming in Python
---

## Managing Packages in Anaconda

The Anaconda Navigator interface can be used to install, remove, and update packages.  To update, start Navigator, click Environments, change the dropdown from `Installed` to `Updateable`, then go through and select packages you wish to upgrade by clicking on the checkbox and selecting "Mark for update" from the dropdown.  The checkbox will change to an arrow.  When you select a package, a green `Apply` button and a red `Clear` button will appear at the lower right.  When you have marked all packages you wish to update, click the `Apply` button.  (It may take a while to complete.)

To install new packages from the Navigator, go to the Environments tab, change the "Installed" dropdown to "Not installed" and wait for the package list to be populated.  Find the package you want, in this illustration one called `dill`, select it (and any other packages you find you might need), then click the green Apply button.

{{< figure src="/courses/python-introduction/imgs/AnacondaPackageManager.png" caption="Anaconda Package Manager" >}}

Conda can be also used from the command line.  In Linux and macOS, the terminal can be used for this.  In Windows, use the Anaconda Command Prompt, which can be accessed through the Apps menu in the Anaconda folder, should be opened for these commands, since it has the correct paths preset.

If you are updating many packages it may be better to do it through a command line. To upgrade a package type

```python
conda update package
```
You can also install packages with either the Navigator interface or with the conda command line.

```python
conda install newpackage
```
Many more options are available.  

{{< figure src="/courses/python-introduction/imgs/Conda.png" caption="Conda from a command line." >}}

## Conda Environments

When you use Anaconda you always have a _conda environment_; the one with which you start is called _base_.  A conda environment is a "bundle" of a Python version, which need not be the same as your base, along with a set of packages installed against that version.  Only one environment can be active at a time (for Linux users, in a given shell) but environments can be activated and deactivated at will.  

A common use for conda environments is to create "sandboxes" to avoid duplication or to use different versions of Python.  For example, Xarray is a general-purpose package but it is particularly aimed at geophysical applications.  Suppose you wish to use it only for a particular application (a homework problem, for example) but do not wish to install it into your main environment.  To create a new environment for your Xarray project, start the Navigator and click on the Environments tab in the upper left.  Click the Create button in the lower pane.  A pop-up will appear.  Name your environment, say `geodat`.  A relatively small number of packages will be installed.  Once created, you can select it from your list of environments, change the dropdown to Installable, and install packages as usual.

From the command line, run
```bash
conda create --name geodat --python=3.11
```
The Python version must be specified.  To switch from the command line run (Mac and Linux)
```bash
source activate geodat
```
Windows
```no-highlight
activate geodat
```

Once activated, run
```
conda install xarray
```
along with other packages you may wish to include.

Conda's [user guide](https://conda.io/projects/conda/en/latest/user-guide/index.html) describes conda's capabilities.

The Conda [cheat sheet](https://docs.conda.io/projects/conda/en/4.6.0/_downloads/52a95608c49671267e40c689e0bc00ca/conda-cheatsheet.pdf) is handy.

Keep in mind that each environment basically copies files into distinct folders, so creating many of them can consume disk storage space rapidly.

### Pip

Pip is different package installer not connected directly to Anaconda or Conda.  It can be used with Anaconda, however, in any conda environments, but Conda will not be aware of packages installed with pip.  However, not all packages are available through Conda so pip is useful.  It is command-line based. Anaconda provides a pip executable for cases where conda cannot be used or the user would prefer pip.  

For most packages it is sufficient to type
```bash
pip install pkg
```
Another option is
```bash
python -m pip install pkg
```
The latter form ensures that the pip is compatible with the Python in use.

As for conda, the exact name of the package must be known.

Documentation for pip is at the official [site](https://pip.pypa.io/en/stable/getting-started/).

### Mamba

Conda can be very slow and, especially with a large number of packages, fail to complete resolving its dependencies before hanging or timing out.  A much faster package manager is [Mamba](https://mamba.readthedocs.io/en/latest/).  It is intended as a drop-in replacement for Conda, but it does not interface with the Anaconda graphical interface; it is a command-line tool. The developers also recommend installing its Python distribution in place of Anaconda. For these reasons, beginners should probably stay with Anaconda and Conda. However, as you gain experience, you may wish to consider Mamba. (In the Mamba documentation, "CLI" stands for "command-line interface" and "CI" stands for "continuous integration," which is an approach to software development.)
