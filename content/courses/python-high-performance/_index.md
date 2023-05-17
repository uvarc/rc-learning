---
title: "High Performance Programming in Python"
type: docs
toc: true
menu:
    hp-python:
        name: High-Performance Python
        weight: 1
---

## Overview

Python, like most interpreted languages, can be very slow. But there are best practices and some programming tricks that can speed it up considerably.  This can make the difference between finishing the work in an acceptable time, or being unable to finish a project.

**First Things First:** Always start with a working, correct code. Make it as clean and readable as possible.

For this tutorial, it is assumed that you have experience with programming in Python. We will explore examples for different **Optimization Strategies**, including

* [Serial Optimization:](serial-optimization) Replacing inefficient code constructs with more efficient ones (single process).
* [Multiprocessing:](multiprocessing) Executing multiple processes on a single computer (shared memory).
* [Distributed Parallelization:](distributed-parallelization) Executing multiple processes across multiple computers (distributed memory, HPC cluster).

- - -

## Setup

To follow along for the [Serial Optimization](#serial-optimization-strategies) and [Multiprocessing](#multiprocessing) examples, you can execute the code examples on your own computer or on UVA's high-performance computing cluster, Rivanna.  Examples described in the last section, [Distributed Parallelization](#distributed-parallelization), are best executed on UVA's high-performance computing platform, Rivanna.

If you are using your local computer, we recommend the Anaconda distribution (<a href="https://www.anaconda.com/distribution/" target="balnk_">download</a>) to run the code examples. Anaconda provides multiple Python versions, an integrated development environment (IDE) with editor and profiler, Jupyter notebooks, and an easy to use package environment manager.

**If you are using Rivanna, follow these steps to verify that your account is active:**

### Check your Access to Rivanna

1. In your web browser, got to <a href="https://rivanna-desktop.hpc.virginia.edu" target="_blank">rivanna-desktop.hpc.virginia.edu</a>.  This takes you to our FastX web portal that lets you launch a remote desktop environment on Rivanna.  If you are off Grounds, you must be connected through the UVA Anywhere VPN client.

2. Log in with your UVA credentials and start a MATE session.  You can find a more detailed description of the Rivanna login procedure <a href="https://www.rc.virginia.edu/userinfo/rivanna/logintools/fastx/" target="_blank">here</a>.
  * **User name:** Your UVA computing id (e.g. mst3k; don't enter your entire email address)
  * **Password:** Your UVA Netbadge password 

3. Starting Spyder (Anaconda's IDE): Open a terminal window and type
```
module load anaconda
python -V
```
You will obtain a response like
```
Python 3.6.6
```
Now type
```
spyder &
```

For Jupyterlab you can use [Open OnDemand](https://rivanna-portal.hpc.virginia.edu).  Jupyterlab is one of the Interactive Apps.  Note that these apps submit a job to the compute nodes.  If you are working on quick development and testing and you wish to use the frontend, to run Jupyter or Jupyterlab on the FastX portal you can run 
```
module load anaconda
anaconda-navigator &
```
This will allow you to choose Jupyter, Jupyterlab, or Spyder.  Jupyter and Jupyterlab will open an instance of Firefox.  You can ignore the error messages as long as Jupyter or Jupyterlab opens and works.  You will not be able to install anything on the Navigator home page that does not have a `launch` button.

Please note that parallelization methods may not work well or at all in Jupyter.
<br>
