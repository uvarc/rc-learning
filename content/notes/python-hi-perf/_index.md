---
title: "High Performance Programming in Python"
type: docs
toc: true
menu:
    hp-python:
        name: High-Performance Python
        weight: 1
---

# Overview

**First Things First:** Always start with a working, correct code. Make it as clean and readable as possible.

For this workshop, it is assumed that you have experience with programming in Python. We will explore examples for different **Optimization Strategies**, including

* [Serial Optimization:](serial-optimization) Replacing inefficient code constructs with more efficient ones (single process).
* [Multiprocessing:](multiprocessing) Executing multiple processes on a single computer (shared memory).
* [Distributed Parallelization:](distributed-parallelization) Executing multiple processes across multiple computers (distributed memory, HPC cluster).

- - -

# Setup for this Workshop

To follow along for the [Serial Optimization](#serial-optimization-strategies) and [Multiprocessing](#multiprocessing) examples, you can execute the code examples on your own computer or on Rivanna.  Examples described in the last section, [Distributed Parallelization](#distributed-parallelization), must be executed on UVA's high-performance computing platform, Rivanna.

All UVA workshop participants should have been provisioned accounts on Rivanna. 

If you are using your local computer, we recommend the Anaconda distribution (<a href="https://www.anaconda.com/distribution/" target="balnk_">download</a>) to run the code examples. Anaconda provides multiple Python  versions, an integrated development environment (IDE) with editor and profiler, and an easy to use package environment manager.

**Before we move on, please follow these steps to verify that your Rivanna account is active:**

### Check your Access to Rivanna

1. In your webbrowser, got to <a href="https://rivanna-desktop.hpc.virginia.edu" target="_blank">rivanna-desktop.hpc.virginia.edu</a>.  This takes you to our FastX web portal that lets you launch a remote desktop environment on Rivanna.  If you are off Grounds, you must be connected through the UVA Anywhere VPN client.

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
<br>

### Get the example scripts 
	
You can copy the example scripts to your Rivanna home directory or download them to your computer. **The code examples are provided for Python 3.** 


* **On Rivanna:** Open a terminal window and run this command to copy the examples to your home directory.  Then change to the directory with the scripts:
```bash
cp -r /share/resources/tutorials/python-hi-perf ~/
cd ~/python-hi-perf
```
      
* **Your computer:**  [Download examples](/notes/python-hi-perf/hi-perf-python.zip) as a .zip archive.  You may wish to make a folder in which to unzip the files.
