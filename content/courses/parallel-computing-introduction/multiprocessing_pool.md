---
title: Pools
date: 2026-08-20:26:29Z
type: book
weight: 2010
menu:
    parallel_programming:
        parent: Multithreaded Programming
---

Multiprocessing consists of several classes. Our first example was from the Process class.  One of the more commonly used classes is the Pool. It is useful for data parallelization across multiple processes. 

As for other members of Multiprocessing, Pool must be run out of `main` so generally does not work within an interpreter.

```python
from multiprocessing import Pool
def f(x):
    return x*x

if __name__ == '__main__':
    pool = Pool(processes=4)
    result = pool.map(f, range(1,41))

    # Print result
    print(result)

    #Close out pool and have processes rejoin
    pool.close()
    pool.join()
```

Note the `join` method to terminate the subprocesses, similar to the fork/join model we have seen.

**Exercise**

Copy the example into a file and run it. If the argument to Pool is empty, it will use the number of logical cores it sees. This is not always desirable, especially in a multi-user or resource-managed environment, so we specify a number. Importing `os` and using `os.getenv` would allow the script to be run by setting an environment variable of our choosing, e.g. `NUM_THREADS` or `SLURM_CPUS_PER_TASK`.  

In this example we created a Pool of four workers (`Pool(processes=4)`). The `pool.map` call submits a workload to the Pool of workers.  The first parameter is the name of the function, in this case `f`, and the second argument defines the sequence of argument(s) that need to be passed to the specified function `f`. Each element of the sequence is passed to f on one of the cores in use.

The `map` function is _blocking_; execution will not continue until the result is returned.  Another version of map, `map_async`, is _nonblocking_. Execution continues while the computations are carried out.  The communication is terminated when `get` is invoked.

```python
if __name__ == "__main__":
   pool = Pool(processes=4)
   result = pool.map_async(f, range(1,11))

   #Do other things

   # Print result
   print(result.get())
```

## Multiple Arguments to Pool

The map method accommodates only one argument to the function.  For Python 3.3 and later, `starmap` is available for multiple arguments.  An efficient way to generate the required iterator of tuples is to use the [zip()](https://docs.python.org/3/library/functions.html?highlight=zip%20function#zip) function.

{{< code-download file="/courses/parallel-computing-introduction/codes/mp_starmap.py" lang="python" >}}

Another set of functions is `apply` and `apply_async`.  The difference between the apply group and map/map_async is that apply returns the result from only one element of the pool.  Like `starmap`, apply supports multiple arguments, but there is no starmap_async so if we need a nonblocking routine equivalent, we should use apply_async.  We will need to collect the results ourselves.

```python
from multiprocessing import Pool

def f(x,y):
  return x**y

if __name__ == "__main__":
   pool = Pool(processes=4)
   results=[]
   for x in range(1,11):
       results.append(pool.apply_async(f, (x,3)))
   allresults=[result.get() for result in results]
   # Print results
   print(allresults)
```
