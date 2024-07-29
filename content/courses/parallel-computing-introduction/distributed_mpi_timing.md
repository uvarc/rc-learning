---
date: "2020-11-17"
title: "Timing and Benchmarking"
weight: 35
---

We should always benchmark our parallel programs to make sure we are utilizing resources effectively, especially if we are running on multi-user systems that may require justifications, proposals, or charges for time.  MPI provides procedures to time all or portions of a program that can be used regardless of how many processes are invoked.  These routines are portable and are the same for all languages that support MPI.

## Wtime and Wtick

`MPI_Wtime` returns a double-precision floating-point number representing the time in seconds that has elapsed since some particular time in the past.  To time parts of the execution, a starting time and an ending time must be specified and the difference obtained.

Since MPI processes run independently, it does not make sense to compare a start time on one process with an end time on another process.  Only differences on the _same_ process are significant.  For an overall time we can often use rank 0.

`MPI_Wtick` returns the resolution, in seconds, for each "tick" of the timer.  This may be of interest for profiling.

The syntax of both procedures is very simple.

### C++
```c++
double startTime=MPI_Wtime();
double tick=MPI_Wtick();
```

### Fortran

Unlike most MPI routines, Wtime and Wtick are functions in Fortran.

```fortran
double precision :: start_time, tick

start_time=MPI_Wtime()
tick=MPI_Wtick()
```

### Python

```python
start_time=MPI.Wtime()
tick=MPI_Wtick()
```

## Scaling and Efficiency Studies

In a previous [chapter](/courses/parallel-computing-introduction/performance_analysis) we learned about _scaling_ and _efficiency_.  Let us apply this to our [random_walk](/courses/parallel-computing-introduction/distributed_mpi_random_walk) program. 

Copy the code from that chapter in your language of choice for this exercise.  Add appropriate timing procedures. It is only necessary to print the result from rank 0. Run the timed random-walk code over 1, 2, 4, and 8 processes for some number of "steps" that will run at least several seconds. Use any plotting package you know to plot speedup versus number of processes/cores.  Add a curve for perfect speedup.  Plot the parallel efficiency versus core number.  (Recall that the theoretical maximum efficiency is 1.)

The random-walk program is "embarrassingly parallel" since there is as yet no communication among processes.  We did not have to use MPI but could have run multiple tests and collected the results.  There is some small overhead in using MPI.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpirandom_walk_timed.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpirandom_walk_timed.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpirandom_walk_timed.py" lang="python" >}}
{{< /spoiler >}}

#### Strong Scaling

In strong scaling, a fixed amount of work is distributed over a varying number of processes. The timed random-walk code is written to implement weak scaling; the number of steps provided is the number each process will compute, so we must adjust the number of steps to keep the amount of work constant over different numbers of processes. The example solution used the Python version with $10^{7}$ steps for the serial code.  That was then divided by 2, 4, and 8 for subsequent runs.

{{< spoiler text="Strong scaling example" >}}
{{< figure src="/courses/parallel-computing-introduction/img/MPI_strong_scaling.png" caption="Graphs of speedup and efficiency for a strong-scaling example" >}}
{{< /spoiler >}}

#### Weak Scaling

Recall that weak scaling increases the amount of work as the process number increases.  Ideally, the quantity of work per process is the same across all processes.  For weak scaling, we must compute the serial time for the _equivalent_ sized problem, so the serial code must be run for $8 \times 10^{7}$, $4 \times 10^{7}$, and $2 \times 10^{7}$ as well as for $10^{7}$ steps.  Plot the scaling compared to perfect scaling of $p$.  Compute and plot the parallel efficiency for these runs.  Was it similar to strong scaling?  Why might this be the case for this particular example?

{{< spoiler text="Weak scaling example" >}}
{{< figure src="/courses/parallel-computing-introduction/img/MPI_weak_scaling.png" caption="Graphs of speedup and efficiency for a weak-scaling example" >}}
{{< /spoiler >}}
