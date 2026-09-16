---
title: Python Concurrency
date: 2026-08-21:26:29Z
type: book
weight: 2090
menu:
    parallel_programming:
        parent: Multithreaded Programming
---

In computing, _concurrency_ is execution and management of multiple tasks more or less simultaneously.  This broad definition covers true parallelism, where each task is executed on its own cpu/core, but also can refer to _context switching_, where multiple processes share the same set of resources, taking turns accessing them.  Those resources can include lightweight kernel threads.  In the specific context of Python, concurrency generally refers to the context switching functions.

Processes that share resources through context switching face the same potential problems as parallel ones, such as race conditions and deadlocks, with an additional risk of _starvation_, when a task thread is unable to access the resource. Normally the operating system handles this problem.

Resource-sharing concurrency works well for tasks such as IO that spend much of their time waiting and need only occasional access.  Tasks that are CPU-bound are better served with true parallelism.

Beginning with Python 3.4, the `concurrent.futures` package was introduced.  This is an interface to enable multiple types of asynchronous processes.  

The package utilizes _executors_. Similarly to MPI executors, these executors manage threads or processes.  Three types are available at this time.

* ThreadPoolExecutor
This creates a pool of threads, generally operating-system level lightweight threads.  It is especially useful for asynchronous tasks that spend much of their time waiting, such as IO.

Syntax
```python
#ThreadPoolExecutor(max_workers=None, thread_name_prefix='', initializer=None, initargs=())
#Usage
import concurrent.futures
executor=concurrent.futures.ThreadPoolExecutor(max_worker=4)
```

* ProcessPoolExecutor
This is a wrapper around the Pool object from Multiprocessing that we have already used. In the future, Pool may be moved more directly into the `concurrent.futures` package.

Syntax
```python
#ProcessPoolExecutor(max_workers=None, mp_context=None, initializer=None, initargs=(), max_tasks_per_child=None)
#Usage
#Like MP Pool, uses number of logical cores it finds
executor = concurrent.futures.ProcessPoolExecutor()
#Lesser number
executor = concurrent.futures.ProcessPoolExecutor(max_workers=nprocs)
```


* InterpreterPoolExecutor
This executor was added in Python 3.14 so may not yet be widely available.  It starts multiple interpreters, each with its own GIL but without as much duplication as is the case for multiprocessing. Only data that can be pickled can be exchanged between these intepreters and mutable quantities cannot be passed.

Syntax
```python
executor=InterpreterPoolExecutor(max_workers=None, thread_name_prefix='', initializer=None, initargs=())
```

For detailed information about concurrent.futures, see its [documentation](https://docs.python.org/3/library/concurrent.futures.html#module-concurrent.futures), including documentation of the methods available in the Executor class and its subclasses.

