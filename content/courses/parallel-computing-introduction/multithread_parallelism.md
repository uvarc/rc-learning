---
title: Parallelism in SMP
date: 2026-07-29T17:26:29Z
type: book 
weight: 1030
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

Just as for distributed-memory programming, parallelism can be expressed in different ways, with the two main ones being _data parallelism_ and _task_parallelism_.

## Data Parallelism

In data parallelism we divide the data into smaller parts and work on each part individually, then if necessary collect results and go to next phase.

In distributed-memory programming, data decomposition must be handled by the programmer by means such as adjusting loop bounds or modifying the algorithm to be valid over the same loop bounds for each of the subdomains. Most of our MPI examples involve this type of data decomposition.

In shared-memory programming, the programmer does not usually explicitly break down the data; the threading library distributes the work across the running threads.

In both cases, the operations performed within the loop should be safe to run concurrently, with no _dependencies_.  MPI can handle dependencies through communication within the loop, but this is not advisable as it is highly inefficient and can lead to errors.  On the other hand, it is fundamental to multithreaded programming that the operations be independent of others in the same loop.


## Task Parallelism

In task parallelism the program performs multiple tasks at the same time on the data.  The tasks must be independent of each other.

Many familiar user applications employ task threading.  A Web browser may use one thread for rendering HTML and another for downloading the data in the background.  A word processor may have separate threads to respond to keystrokes, check spelling, and display the page.

Some authors limit "parallelization" to data parallelism and refer to task parallelism as _concurrency_. Tasks run in their own threads concurrently but are not necessarily as tightly coordinated as in data parallelism.
