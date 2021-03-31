---
title: "Strategies for Single-Core Optimization"
type: docs
toc: true
weight: 2
menu:
    hp-python:
        parent: High-Performance Python
        weight: 2
---

Python, like most interpreted languages, can be very slow. But there are best practices and some programming tricks that can speed it up considerably.  This can make the difference between finishing the work in an acceptable time, or being unable to finish a project.

## Profiling & Timing

The first step is usually to _profile_ the code.  Profiling can help us find a program's bottlenecks by revealing where most of the time is spent.  Keep in mind that most profilers work per function.  If you do not have your code well separated into functions, the information will be less useful.
Profilers are statistical in nature.  They query the program to find out what it is doing at a snapshot during the run, then compile the results into a kind of frequency chart.  


### Python Profiler in the Spyder IDE
The Anaconda Spyder IDE provides an integrated Profiler that is easy to use. To profile your Python code follow these steps:

1. Open the Python script in the Editor pane.  If your program is comprised of multiple files, open the script that contains the the main function.
2. In the Spyder menu, go to `Run` -> `Profile`.

The results will be shown in the `Profiler` pane. 

**Exercise:** 
Open the `fibonacci.py` file and execute it with the Spyder Profiler.  The code deliberately uses an inefficient algorithm.  Let's look at the output in the `Profiler` pane.  What function was called most frequently and has the largest cumulative run time?

![](/notes/python-hi-perf/fibonacci-profiler.png)

A more detailed description of the Profiler option for Spyder can be found <a href="http://docs.spyder-ide.org/profiler.html" target="_blank">here</a>.

#### Using the cProfile and pstats Modules in your script

Python includes the `cProfile` and `profile` packages.  In Python 2, profile is a pure Python module with significant more overhead than the C extensions of cProfile (<a href="https://docs.python.org/2/library/profile.html" target="_blank">read the details</a>).  In Python 3, `profile` is `cProfile` by default.  These packages can be used from the command line or in your Python scripts.

You can use the `cProfile` and `pstats` modules in your script to profile specific functions and save the profiling results to a file. Let's assume that we have a module `MyModule` that defines a function `myFunc` to profile: 
```python
import MyModule
import cProfile
cProfile.runctx("MyModule.myFunc()", globals(), locals(), "Profile.result") 
```
Here `globals()` defines a set of global variables to be set for the profiling run.  The `locals()` is a dictionary for arguments passed to the specific function to be profiled, i.e. `MyModule.myFunc`. The last argument, `Profile.result`, defines the file that the profiling output is written to.

The following code snippet can be used to implement profiling of the `fib_seq` function in the `fibonacci.py` script.
```python
import cProfile,pstats

cProfile.runctx("fib_seq", globals(), {'n':20}, "Profile.result")
s = pstats.Stats("Profile.result") 
s.strip_dirs().sort_stats("time").print_stats()
```
Note the passing of `{'n':20}` to pass local arguments to the `fib_seq` function. The profiling stats are read from the `Profile.result` file, the path information of the involved Python modules is removed, and the output is sorted by `time` values before it is printed.

**Other Tools**

If you are running on your own system, you can install the `snakeviz` package.  This enables you to view profiler output in a browser window.  First you must generate the profile file.  You can then invoke snakeviz from the command line with
```
snakeviz Profile.result
```

To use snakeviz within an iPython shell, use the "magics"
```python
%snakeviz code_line
```
for a single line, or
```python
%%snakeviz
a line
for i in stuff:
   more
```
for multiple lines.

To use snakeviz from within a Jupyter notebook, first load it
```python
load_ext snakeviz
```

Another option in iPython or Jupyter is `prun`.
```python
%prun
```
This runs the Cprofile profiler.

You can also install the `line_profiler` and `mem_profiler` tools.
The line_profiler tool provides a special version of the Python interpreter, `kernprof` and defines a decorator `@profile`.  To use it from the command line, add the decorator to any function you wish to profile.
```python
@profile
def fib(n):
    # from http://en.literateprograms.org/Fibonacci_numbers_(Python)
    if n < 2:
        return n
    else:
        return fib(n-1) + fib(n-2)

@profile
def fib_seq(n):
    results = [ ]
    if n > 0:
        results.extend(fib_seq(n-1))
    results.append(fib(n))
    return results
```

Another popular IDE (integrated development environment) for Python is _Pycharm_ by JetBrains.  The profiler option is only available in the Professional (paid) version, but some UVA departments have a license for this version.

### Timing

Often we want to know exactly how long a particular code, or portion of the code, ran.  Profiling adds some execution overhead and works only for functions.  For finer-grained data, we must measure the elapsed time directly.

The `time` module is available for longer segments of code.

```python
import time
start_time=time.clock()
# do stuff
end_time=time.clock()
print ("Elapsed time is ",end_time-start_time)
```

Python 3 offers the `timeit` module for short snippets.
```python
import timeit
print(timeit.timeit('"-".join(str(n) for n in range(100))', number=10000))
```
Since a single repetition of a command in Python may run too quickly to measure easily, we specify a number of repetitions (`number=10000`).  

## Serial Optimization Strategies

We can represent the optimization process with a flowchart:

{{< diagram >}}
graph TD;
A(Profile or time) --> B(Tune the slowest section);
B --> C{Performance increase?};
C -- Yes --> D(Go to next slowest);
C -- No --> E(Try a different solution);
D --> A
{{< /diagram >}}

There are many approaches to speeding up sections, some specific to Python and some more generic.  In this section we will consider several possibilities.

### Avoid for Loops

#### Strategy 1. 
Use Python-specific constructions such as list comprehensions.  Use generators whenever possible.  Functionals such as `map()` can also be faster than for loops. 

List comprehensions compress a for-loop into a single line, with an optional conditional.
```python
import math
a_list=[-10.,-8.,-6.6,-3.,0.,2.3,4.5,7.1,8.9,9.8]
y = [x**2 for x in a_list]
sqrts = [math.sqrt(x) for x in a_list if x>=0]
```

The three functionals take a function as their first argument, and an iterator as the second argument.
The `map()` functional remains available in Python 3, along with `filter()`. The `reduce()` functional must be imported from the `functools` module.  In Python 3 they return iterators and must be cast to a list if desired.  They may be used with a predefined function; they are frequently used with "anonymous" or _lambda_ functions.
```python
y2 = list(map(lambda x:x**2,a_list))
sqrts2 = list(map(lambda x:math.sqrt(x),filter(lambda x:x>=0,a_list)))
```

A _generator_ is a function that returns an iterator.  Rather than creating all the results and storing them, they create but do not store values, returning them as they are needed.  In Python 3, range is a generator.  You can write your own generators by using `yield` rather than `return`.  Each value must be "yielded" as it is produced.
```python
def square(x):
    yield x**2
```

A list comprehension can be converted to a generator by using parentheses rather than square brackets.

```python
yg = (x**2 for x in a_list)
```
The result is a _generator object_.  This can save both memory and time.

**Example**
The following code tests the speed of map, list comprehension, and loop.
```python
import random
import timeit

values = [random.randrange(100) for _ in range(100000)]

def dummy(x):
    return x**2+42.

def with_map():
    return list(map(dummy, values))

def with_comp():
    return [dummy(x) for x in values]

def with_loop():
    result = []
    for x in values:
        result.append(dummy(x))
    return result

print(f'Time for map {timeit.timeit(with_map, number=100):.4f}')
print(f'Time for comprehension {timeit.timeit(with_comp, number=100):.4f}')
print(f'Time for loop {timeit.timeit(with_loop, number=100):.4f}')
```
The result on one particular system:
```python
Time for map 4.9825
Time for comprehension 5.3274
Time for loop 5.6446
```

#### Strategy 2. Convert everything you can to use NumPy array intrinsics.

NumPy provides a large library of functions on NumPy arrays that take the place of loops.  This is referred to as _vectorizing_ a code.

**Exercise:** 
Nested for loops are **very inefficient** (`loops.py`)
{{% code file="/notes/python-hi-perf/loops.py" lang="python" %}}

Eliminating for loops is **much faster** (`aops.py`)
{{% code file="/notes/python-hi-perf/aops.py" lang="python" %}}

**Results** with Python 3.6.9 on one particular system:

```
loops.py  1.197 sec 
aops.py  .211 sec 
```

**Extreme Example**
```python
# assume numpy array with n x n elements
for i in range(1,n-1):
      for j in range(1,n-1):
           u[i,j]=0.25*(u[i+1,j]+u[i-1,j]+u[i,j+1]+u[i,j-1])
```
Replace with a single line
```python
u[1:-1,1:-1]=0.25*(u[2:,1:-1]+u[:-2,1:-1]+u[1:-1,2:]+u[1:-1,:-2] 
```

**Example**
Our "dummy" function is a ufunc, so we can run a trial with little modification to the previous code.  The "setup" code is not timed by timeit.
{{% code file="/notes/python-hi-perf/replace_forloop.py" lang="python" %}}

The difference is remarkable.  Remember that times for different runs may vary somewhat even on the same system, but the basic result will be similar.
```
Time for map 5.0804
Time for comprehension 5.4944
Time for loop 5.9716
Time for numpy 0.0885
```

**More Information**

* <a href="https://docs.python.org/3/tutorial/datastructures.html#list-comprehensions" target="_blank">List comprehensions</a>
* <a href="https://docs.python.org/3/library/itertools.html?highlight=map%20reduce" target="_blank">map/reduce/itertools</a>

### Avoid Copying

**Bad:**
```python
s = ""
for x in mylist:
    s += string_function(x)
```

**Better:**
```python
slist = [string_function(el) for el in mylist]
s = "".join(slist)
```

Not only does the first version have a for loop, but since strings are immutable each concatenation requires copying.  A join is much faster.  
**Note:** string concatenation is faster in Python 3 than in Python 2.

### Minimize Use of Dynamically Sized Objects

Use dynamically sized objects when appropriate, but do not append if you don’t have to do so.  Especially avoid inserting.

### Simplify Mathematical Expression

Mathematical functions are very slow, in general.  When possible, simplify mathematical expressions.  For example,
$$ e^a e^b=e^{a+b} $$
Reducing two exponential evaluations to one can save a significant amount of time, especially if the expression is repeated many times.

### Use Functions

Due to some quirks of Python, functions are faster than straight code.  

This implies you should use a main() function even if you never import your file as a module:

```python
def main():
     solve_problem()

if __name__==”__main__”:
      main()
```

### Concluding Advice for Serial Optimization

* Do not sacrifice readability for optimization.  Human time is much more expensive than computer time.
* Do simple optimizations first.  Profile before undertaking extensive optimization efforts.
* Use timing functions to obtain finer-grained information about bottlenecks as needed.
