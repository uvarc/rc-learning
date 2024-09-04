---
title: Setting Up Your Environment
toc: true
type: docs
draft: false
weight: 11
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
        parent: Introduction to Programming in Python
---

We will start by installing the Anaconda environment from [Anaconda Inc](https://anaconda.com).  Anaconda is widely used because it bundles a Python interpreter, most of the popular packages, and development environments. It is cross platform and freely available. [Download](https://www.anaconda.com/products/individual#Downloads) and install the Anaconda Distribution product appropriate for your operating system (Windows, MacOS, or Linux). The standard download will install some variation of Python 3 (version 3).

There are two somewhat incompatible versions of Python; version 2.7 is obsolete but still sometimes used.  Version 3 is the supported version.  We will use Python 3, preferably version 3.9 or above.  If you find that you need to use Python 2.7, Anaconda makes it easy to run both versions side-by-side through _conda environments_.

Anaconda will normally automatically detect your operating system but you should double check to be sure you are downloading the correct version.  On Windows and Mac, after the download is complete, install Anaconda in the usual way for your operating system.  When installing, we strongly recommend staying with the defaults unless you are experienced and very sure you want to make changes. The installer will encourage you to sign up for the Anaconda cloud service; you may ignore that and dismiss the popups. You can do this later if you wish. You may also ignore the suggestions for "reading" or "training."

On Linux, the installation is a shell script that you must run with
```bash
bash ~/Downloads/Anaconda3-<date>-Linux-x86_64.sh
```
Be aware that it will alter your `.bashrc` or other shell resource file. 

Multi-user Linux systems such as [Rivanna](https://www.rc.virginia.edu/userinfo/rivanna/overview/) may have Anaconda or other Python preinstalled.  See the instructions for your local system, such as [ours](https://www.rc.virginia.edu/userinfo/rivanna/software/anaconda/).

Once you have installed Anaconda, [find the Navigator application](https://docs.anaconda.com/anaconda/user-guide/getting-started/) in the appropriate menu for your operating system.  You should see a workspace similar to this

{{< figure src="/courses/python-introduction/imgs/AnacondaNavigator.png" caption="The Anaconda Navigator home screen." >}}

The Navigator will present many options that may change over time, most of which are not particularly useful to beginners. Not all are installed by default but can be installed with a click.  In this course we will use **Spyder** and **JupyterLab**, which should already be available.  If not, click the button to install the package.

On Windows you can start the Anaconda Navigator, as well as some other Anaconda tools, from the App menu.

If you are using Anaconda from a personal Linux or MacOS system and are comfortable using the command line from a terminal, you can start Spyder or  JupyterLab with
```bash
jupyter-lab &
spyder &
```
Alternatively, you can run the Navigator if you wish to start from there.
```bash
anaconda-navigator &
```

MacOS users in particular may find that Navigator may be very slow or tend to hang when started from the Launchpad.  Starting from a terminal can avoid this problem.

Both JupyterLab and Spyder may be fairly slow to start, so be patient.  JupyterLab will start an instance or a new tab of the default Web browser and run there. Spyder is a standalone application.

When using Anaconda, never install Jupyter/JupyterLab or Spyder independently; you can upgrade them with the Conda package manager.
