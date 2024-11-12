---
title: "High Performance Programming in Python"
type: docs
toc: true
date: "2020-11-17T00:00:00"
tags: ["Programming","Python","HPC"]
categories: ["Programming","Python","HPC"]
menu:
    hp-python:
        name: High-Performance Python
        weight: 1
---

Python, like most interpreted languages, can be very slow. But there are best practices and some programming tricks that can speed it up considerably.  This can make the difference between finishing the work in an acceptable time, or being unable to finish a project.

**First Things First:** Always start with a working, correct code. Make it as clean and readable as possible.

For this tutorial, it is assumed that you have experience with programming in Python. We will explore examples for different **Optimization Strategies**, including

* [Serial Optimization:](serial-optimization) Replacing inefficient code constructs with more efficient ones (single process).
* [Multiprocessing:](multiprocessing) Executing multiple processes on a single computer (shared memory).
* [Distributed Parallelization:](distributed-parallelization) Executing multiple processes across multiple computers (distributed memory, HPC cluster).
* [GPU Acceleration:](gpu-acceleration) A minimal introduction to programming for a GPU with Python.

- - -

## Setup

To follow along for the [Serial Optimization](#serial-optimization-strategies) and [Multiprocessing](#multiprocessing) examples, you can execute the code examples on your own computer or on UVA's high-performance computing cluster.  Examples described in the last section, [Distributed Parallelization](#distributed-parallelization), are best executed on UVA's high-performance computing platform.

If you are using your local computer for your personal applications, not related to work, you can install the Anaconda distribution (<a href="https://www.anaconda.com/distribution/" target="balnk_">download</a>) to run the code examples. Anaconda provides multiple Python versions, an integrated development environment (IDE) with editor and profiler, Jupyter notebooks, and an easy-to-use package environment manager.  If you will or might use the installation for work, or just prefer a more minimal setup that you can more easily customize, we suggest Miniforge (https://github.com/conda-forge/miniforge).

**If you are using UVA HPC, follow these steps to verify that your account is active:**

### Check your Access to UVA HPC

1. In your web browser, go to <a href="https://fastx.hpc.virginia.edu" target="_blank">fastx.hpc.virginia.edu</a>.  This takes you to our FastX web portal that lets you launch a remote desktop environment on a frontend.  If you are off Grounds, you must be connected through the UVA Anywhere VPN client.

2. Log in with your UVA credentials and start a MATE session.  You can find a more detailed description of the FastX login procedure <a href="https://www.rc.virginia.edu/userinfo/rivanna/logintools/fastx/" target="_blank">here</a>.
  * **User name:** Your UVA computing id (e.g. mst3k; don't enter your entire email address)
  * **Password:** Your UVA Netbadge password 

3. Starting Spyder: You must first activate an environment and install Spyder into it.  Open a terminal window and type
```
module load miniforge
python -V
```
You will obtain a response like
```
Python 3.11.3
```
If your environment does not include it, install the package
```
conda install spyder
```
Now type
```
spyder &
```

For Jupyterlab you can use [Open OnDemand](https://ood.hpc.virginia.edu).  Jupyterlab is one of the Interactive Apps.  Note that these apps submit jobs to compute nodes. If you need to use Jupyterlab outside of the OOD interactive app, you should install it into your environment similarly to installng Spyder.
```
conda install jupyterlab nbconvert
```
The `nbconvert` paackages allows Jupyter to export your cells to various formats, including Python scripts.  You can then invoke it with
```
jupyter-lab &
```
It will open in the default Web browser.

Please note that parallelization methods may not work well or at all in Jupyter.
<br>
