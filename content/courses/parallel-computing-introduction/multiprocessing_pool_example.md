---
title: Example Application with Pool
date: 2026-08-20:26:29Z
type: book
weight: 2060
menu:
    parallel_programming:
        parent: Multithreaded Programming
---

When we first started parallelizing code, we used a short script that computed by by the _Monte Carlo_ method.  

Map requires an iterator for its second argument. We will manually divide the total number of "data throws" into chunks of roughly equal size on each process and store the result into a list _myNumPoints_. The Pool map method will then distribute the elements of the list, one to each cpu.  This is called **load balancing** in parallel computing terms.  Maximum efficiency generally occurs when each process performs approximately the same quantity of work.
We also do not hard-code the number of processes, but will set an environment variable `NUM_PROCS` outside to select the core count.

{{< code-download file="/courses/parallel-computing-introduction/codes/mp_montecarlopi.py" lang="python" >}}

#### Scaling

Most modern personal computers, including laptops, are multicore.  If you are running on your own computer, test the code for a fairly small number of "dart throws." You may change ncpus to a fixed integer corresponding to your computer's core count.  Start with 10000 and increase to 100000, then to 1000000.  You may find that for a small number of throws, the serial time is faster than the multicore time.  This is due to _overhead_, which includes the additional time required to set up the multiple processes and communicate between them.  The result on one computer running Linux was
```no-highlight
python mp_montecarlopi.py 10000
ncpus=4
Points: [2500, 2500, 2500, 2500]
3.1728
Parallel time on 4 cores:0.0011
3.1416
Serial time:0.001

 python mp_montecarlopi.py 100000
ncpus=4
Points: [25000, 25000, 25000, 25000]
3.1366
Parallel time on 4 cores:0.0055
3.13592
Serial time:0.0099

python mp_montecarlopi.py 1000000
ncpus=4
Points: [250000, 250000, 250000, 250000]
3.1393560000000003
Parallel time on 4 cores:0.0422
3.140852
Serial time:0.1007

python mp_montecarlopi.py 10000000
ncpus=4
Points: [2500000, 2500000, 2500000, 2500000]
3.1408840000000002
Parallel time on 4 cores:0.2585
3.1421124
Serial time:0.9626
```
As we might expect, the time for the serial run increases roughly linearly with the number of points.  The parallel time seems to obey the same rule after the first test run; for larger runtimes the additional time to set up Multiprocessing becomes less significant.  The value of $\pi$ also becomes more accurate as the number of "throws" increases.

We are increasing the amount of data per core without changing the number of cores. This is neither strong scaling nor weak scaling.

**Exercise**

Run a strong scaling (same amount of work over a different number of cores) test and a weak scaling test (increase the amount of work but keep the amount per core the same).  Plot the scaling results for the parallel timings.

**Sample Results**

{{< spoiler text="Stong scaling" >}}
{{< figure src="/courses/parallel-computing-introduction/img/mp_montecarlopiStrongscaling.png" caption="Strong scaling for Multiprocessing example" alt="Plot showing results for strong scaling of the Multiprocessing example" >}}
{{< /spoiler >}}

{{< spoiler text="Weak scaling" >}}
{{< figure src="/courses/parallel-computing-introduction/img/mp_montecarlopiWeakscaling.png" caption="Weak scaling for Multiprocessing example" alt="Plot showing results for weak scaling of the Multiprocessing example" >}}
{{< /spoiler >}}

