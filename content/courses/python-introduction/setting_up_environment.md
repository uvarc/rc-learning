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

In this course we will use the free [Miniforge](https://github.com/conda-forge/miniforge) distribution of Python and Python packages.  Miniforge is available for Windows, MacOS, and Linux.  Download the latest release version for your operating system from the above page, and follow the instructions to install the base.  Mac users: "Apple Silicon" refers to the `M` chips on newer Macs.  Please be sure to install the standard Miniforge3 and _not_ Miniforge-pypy.

There are two somewhat incompatible versions of Python; version 2.7 is obsolete but still sometimes used.  Version 3 is the supported version.  We will use Python 3, preferably version 3.9 or above.  If you find that you need to use Python 2.7, Miniforge makes it easy to run both versions side-by-side through _environments_.  At the time of this writing, Miniforge3 installs Python 3.10.4 by default with Release 23.0-0, but will later switch to Python 12.0. 

When installing, we strongly recommend staying with the defaults unless you are experienced and very sure you want to make changes. In particular, it is best to install it "Only for You."  

On Linux, the installation is a shell script that you must run with
```bash
bash ~/Downloads/Miniforge3-Linux-x86_64.sh 
```
Be aware that it will alter your `.bashrc` or other shell resource file unless you request that it not do so. 

Multi-user Linux systems such as [UVA HPC](https://www.rc.virginia.edu/userinfo/rivanna/overview/) may have Miniforge or other Python distributions preinstalled.  See the instructions for your local system, such as [ours](https://www.rc.virginia.edu/userinfo/rivanna/software/anaconda/). Typically, on a system such as UVA's HPC resource, you must load a [module](https://www.rc.virginia.edu/userinfo/hpc/software/modules/) before you can use the distribution.

On Windows, Miniforge will install a _miniforge prompt_ which will appear in your Apps menu under the Miniforge folder.  It can be used much like a terminal on Linux or Mac.  It will be aware of python, pip, mamba, and conda commands, but unless you add those to your personal PATH environment variable, which is usually not recommended, other `CMD` or `PowerShell` windows will not find those commands.

The default installation of Miniforge creates a _base environment_ consisting of a very minimal number of packages.  It uses a [package manager](courses/python-introduction/packages_environments) to install further packages.  You can choose `conda` or `mamba`.  In general, we recommend mamba as it is usually much faster than conda.

If you wish to install some of the more popular packages into your base environnment, open a terminal (Linux and MacOS) or a Miniforge prompt (Windows).  A good "starter pack" would be NumPy, Matplotlib, Pandas, Seaborn, Jupyterlab, and Spyder.  We would also like to upgrade our Python from the 3.10.4 in our current release version.  At your prompt type, all on one line,
```bash
(base)$mamba install python=3.11 numpy pandas scipy mkl seaborn numba jupyterlab ipython ipywidgets spyder
```
This will install many more packages than just those requested, since each will have packages on which they depend. 

You may also decide to keep your base minimal and set up 
