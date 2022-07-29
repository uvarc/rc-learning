---
title: Setting Up Your Environment
toc: true
type: book
draft: false
weight: 10
---

## Setting up Your Environment

The Anaconda environment from [Anaconda Inc.](https://anaconda.com) is widely used because it bundles a Python interpreter, most of the popular packages, and development environments. It is cross platform and freely available. [Download](https://www.anaconda.com/products/individual#Downloads) and install the Anaconda Distribution product appropriate for your operating system (Windows, Mac OSX, or Linux).  There are two somewhat incompatible versions of Python; version 2.7 is deprecated but still fairly widely used.  Version 3 is the supported version.  We will use Python 3 but if you know you need to use Python 2.7, you can download that instead.  Anaconda even makes it relatively easy to install both versions.

Once you have installed Anaconda, [find the Navigator application](https://docs.anaconda.com/anaconda/user-guide/getting-started/) in the appropriate menu for your operating system.  
You should see a workspace similar to this
![Anaconda-Navigator](/courses/python_introduction/imgs/AnacondaNavigator.png)
with several options for working environments, some of which are not initially installed.  In this course we will use Spyder and Jupyterlab, which should 
already be available.  If not, click the button to install the package.

You may also use Anaconda on UVA's high-performance computing cluster, Rivanna.  When this short course is presented, in-person attendees should have been given temporary accounts in advance; 
readers working through this tutorial for self-study who do not already have an account can obtain one through their faculty advisor.
If you are a new user, please see our [tutorial](/slides/rivanna-intro) for an introduction to using Rivanna.  We recommend using [Open OnDemand](https://rivanna-portal.hpc.virginia.edu) (requires Netbadge).  You can use Jupyterlab directly as an Interactive App.  To use Spyder, request a Desktop interactive app.  Please select the `instructional` partition in the drop-down, and fill in `rivanna-training` as the Allocation Group.  If using the Desktop app, once it connects you must start a terminal and type
```bash
module load anaconda
spyder &
```
To use Jupyterlab through the Desktop, or to switch back and forth, in another terminal run
```bash
module load anaconda
jupyter-lab &
```

If you are using Anaconda from a personal Linux system, you can run the same 
commands without loading any modules.  You may also run the Navigator if you wish
```bash
anaconda-navigator &
```

Both these applications may be fairly slow to start, so be patient.  Jupyterlab will start an instance or a new tab of the default Web browser and run there.  Spyder is a standalone application.

When using Anaconda, never install Jupyter/Jupyterlab or Spyder independently; you can upgrade them with the Conda package manager.
