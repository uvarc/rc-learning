---
title: Python Multiprocessing
date: 2026-07-30:26:29Z
type: book
weight: 2000
menu:
    parallel_programming:
        parent: Multithreaded Programming
---

We have been studying OpenMP with Python.  However, this is new and is not how Python has historically handled multithreading.

## The GIL

Standard Python implements a GIL (global interpreter lock). Threads cannot be started within a single interpreter.  The GIL was implemented because Python was not originally designed to be thread safe, and could encounter race conditions if multiple threads could access some of its internal variables.

True threading can be simulated, but it’s slow. Some functions in certain packages are able to release the GIL temporarily, but in a typical Python program, it cannot be assumed that multithreading will be efficient.

Python does provide a threading package `threading`, which can spawn threads as long as they do not conflict with core Python processes, but the GIL still interferes and often makes using this library effectively a challenge. With newer Python versions able to remove the GIL, and Python interest groups joining the OpenMP committee, some standardized form of true threading will likely arise. Python 3.13 and higher can be built without the GIL, though 3.14 and higher is recommended at the current time for free-threading builds.

The direct solution to the GIL was to start more interpreters as subprocesses and enable them to communicate. The package that enables this is called `Multiprocessing` and is standard in Python 2.7 and up.

This sounds similar to distributed-memory programming, but it does not support communications across different nodes and should be regarded as a form of SMP programming.

## Starting Processes

Different operating systems may support different methods of creating new processes, and some may support more than one.  Multiprocessing starts new processes by one of the following means:

### Spawn

The parent process can _spawn_ new processes. The new processes inherit only those of the system resources necessary to run.  This is the default on Windows and MacOS.

### Fork

This is similar to forking threads. The parent process forks new processes, each of which inherits all the resources (memory, etc.) of the parent. This can use up considerable system resources and is risky if the processes are themselves trying to thread.  This is the default on Linux through Python 3.13.

### Forkserver

The parent process spawns a _server_ process with which it communicates. When new processes are requested the forkserver instantiates them. This is the default on Linux for Python 3.14 and up.

Full documentation is available at the [official site](https://docs.python.org/3/library/multiprocessing.html)

**Example**

This example show illustrates the Process class, which was intended to be similar to the threading package.

Import the package
```python
from multiprocessing import Process
```

Define a function
```python
def f(name):
    print(f"Hello from {name}")
```

It is important to realize that Multiprocessing can _only_  run as main.  

```python
if __name__ == '__main__':
ncpus=4
for i in range(ncpus):
    p=Process(target=f,args=(i,))
    p.start()
    p.join()
```

**Exercise**

Type the example into a file and run it. Vary `ncpus` based on the number of cores available to you.
