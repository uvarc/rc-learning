---
title: OpenACC Example and Exercise
date: 2026-09-2T17:26:29Z
type: book 
weight: 4095
menu: 
    parallel_programming:
        parent: Accelerator Programming   
---

A great deal of information about programming in OpenACC can be found in the OpenACC Programming and Best Practices [Guide](https://openacc-best-practices-guide.readthedocs.io/en/latest/index.html).

Based on this we can return to our familiar Jacobi iteration example to implement it on a GPU.

{{< spoiler text="C" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/oacc_laplace.c" lang="c" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/timer.h" lang="c" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/oacc_laplace.f90" lang="fortran" >}}
{{< /spoiler >}}

**Exercise**

Run the NVIDIA-written example for your language of choice.  If you are using the NVIDIA HPC suite, compile with `acc=gpu` and run it, then recompile with `acc=multicore` and compare the runtimes.
