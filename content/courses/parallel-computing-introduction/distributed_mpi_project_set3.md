---
title: "MPI Project Set 3: Jacobi Implementation"
toc: true
type: docs
weight: 200
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

## Project 7

Implement a serial Jacobi code to solve the two-dimensional Poisson equation with boundary conditions as described in our example, namely

```nohighlight
u[0,:]=0.
u[nr+1,:]=100.
u[:,0]=100.
u[:,nc+1]=100.
```

Do not assume a square grid, but you may use one for testing, such as 500x500. Write your output to a file in your preferred format.

_Suggestions_.  

It is usually not necessary to check for convergence at every timestep. Make it  a variable and experiment with different values.  Examine whether it affects the time required for a run.

Python programmers: once your code is working, try to use NumPy array operations with appropriate ranges to replace the double `for` loop.  It should speed up the code substantially.

Fortran programmers: when writing double `do` loops over array indices, whenever possible arrange the loops in order of the indices from _right_ to _left_, for cache efficiency. Correct loop ordering can be approximately a factor of 50 faster than incorrect for a double loop.

Use whatever plotting package you know to make a contour plot of the result. If you do not have a preference, you may use `contour.py` below. It assumes a single output file and will transpose the result for Fortran, if output is written by column rather than row, so the orientation is the same.

When plotting, the top of an array (row 0) is the bottom of the plot.

{{< spoiler >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/contour.py" lang="python" >}}
{{< /spoiler >}}

### Example solutions

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/heatedplate.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/heatedplate.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/heatedplate.py" lang="python" >}}
{{< /spoiler >}}

## Project 8

Using the halo exchange code we have already seen, plus at least one collective communication, to parallelize your heated-plate code. Do not declare a single lage global array; break up the domain as we have learned (row-wise for C++ and Python, column-wise for Fortran).  For output, have each rank write its set of rows or columns, labeling the file with its rank ID.  

Example syntax:
```c++
  fname=argv[2]+to_string(rank);
```
Fortran
```fortran
  write(fname,'(a,i4.4)') filename(1:len_trim(filename)),rank
```
Python
```python
filename = filename + str( rank )
```  
To plot the results you will have to stitch the files together appropriately. If you have no other preference you may use the `contour-ranks.py` code below. Add the `-f` command-line option for Fortran. The script must be given an argument that is the "base" of the filenames, and all the output files must be in the same folder and numbered appropriately.

{{< spoiler >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/contour-ranks.py" lang="python" >}}
{{< /spoiler >}}

Feel free to use the example solutions provided as your basis, but we recommend that you attempt to write your own serial version first.

_Hints_
1. You will need to modify the stopping criterion for the update loop. There are several options to do this. One is to use the maximum number of iterations as the criterion, then break out of the loop when the maximum difference in all the ranks is less than the specified tolerance.
2. Physical boundary conditions generally must be reapplied at each iteration to the "solution" matrix `u`. A subprogram to do this might be a good idea.
3. You need to do a full swap of the "current" solution `u` and the "updated" solution `w` at each iteration, which is why the BCs must be reapplied.
4. Check that your program works for a single process.

### Example solutions

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpiheatedplate.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpiheatedplate.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/mpiheatedplate.py" lang="python" >}}
{{< /spoiler >}}

## Project 9

### Scaling Studies

We have discussed weak and strong [scaling](performance_analysis.md). If you have not already done so, add timing to your heated plate solution in the language of your choice. Use `MPI_Wtime()` since it will generate uniform timings regardless of your programming language.  It is sufficient to instrument the `while` loop since nearly all the time will be spent there.

1. The example solutions show how to do strong or weak scaling. You can do only one at a time.  Weak scaling will increase the size of the global domain in a way that will make it rectangular rather than square, but that doesn't matter for our study.

2. Start with strong scaling, dividing up the same amount of work over an increasing number of processes.  Choose a value for the number of rows or columns that will be evenly divisible by 8. Run your code for 1, 2, 4, and 8 processes.  Make a plot of time versus number of processes.  Compute the parallel efficiency for each run.

3. Repeat for weak scaling. 

Make sure you are using idle cores for this test, or your results will not reflect the time spent on your processes. If you have access to a high-performance computing cluster with a resource manager (queueing system), submit each test case as a separate job. Are the results what you expected? 
